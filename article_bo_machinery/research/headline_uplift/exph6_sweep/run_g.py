#!/usr/bin/env python3
"""G-sweep orchestrator (protocol: DESIGN.md). Resumable per wave.

Usage:
  run_g.py smoke                 # the 5 pre-named timing cells, 1 seed
  run_g.py fast [CLASS ...]      # fast arms, all budgets, given classes
  run_g.py gp   [CLASS ...]      # GP arms per DESIGN budgets
Classes: A B C D E F (default: all). Completed (arm, bench, budget, seed)
keys in results.jsonl are skipped, so waves survive recycles.
"""
import json
import os
import subprocess
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
RESULTS = HERE / "results.jsonl"
FAILURES = HERE / "failures.log"
SCRATCH = Path(os.environ.get("SCRATCH", "/tmp/claude-0/-home-user-BayesianOptim-BASS/"
                              "d8cc14fb-4c54-5880-818f-3a67a8836a1b/scratchpad"))

CLASSES = {
    "A": ["cat_ackley_d3_L5", "cat_ackley_d5_L5", "cat_ackley_d6_L11",
          "catf_rastrigin_d4L7", "catf_griewank_d5L7"],
    "B": ["catf_rosen_d4L7", "catf_michal_d5L9", "catf_schwefel_d4L9"],
    "C": ["nk_n20k2", "nk_n20k8", "maxcut_n20", "labs_n25"],
    "D": ["pest_control", "contam_2p25"],
    "E": ["ml_rf_digits", "ml_svm_digits", "ml_gb_bc", "ml_mlp_wine",
          "yahpo_rpart_41138", "yahpo_rpart_40981", "yahpo_ranger_1489"],
    "F": ["func2C", "func3C"],
}
ALLB = [b for c in "ABCDEF" for b in CLASSES[c]]
FAST_ARMS = ["random", "optuna-tpe", "optuna-tpe-3.6", "hyperopt-tpe", "smac"]
GP_ARMS = ["optuna-gp", "skopt-gp", "ax"]
SMAC_COVER = set(b for c in "ABCDF" for b in CLASSES[c])   # DESIGN coverage limit
BUDGETS = [20, 40, 80, 160]
GP160 = {"cat_ackley_d5_L5", "catf_rosen_d4L7", "catf_michal_d5L9", "nk_n20k2",
         "pest_control", "ml_svm_digits", "yahpo_rpart_41138", "func2C"}
SEEDS = list(range(4001, 4026))
BIGDIM = {"pest_control", "contam_2p25", "labs_n25"}
ENV = {**os.environ, "OMP_NUM_THREADS": "1", "MKL_NUM_THREADS": "1",
       "OPENBLAS_NUM_THREADS": "1"}
lock = threading.Lock()


def cap_for(arm, bench, budget):
    if arm in GP_ARMS and (budget == 160 or bench in BIGDIM):
        return 2700
    return 1200


def done_keys():
    keys = set()
    if RESULTS.exists():
        for line in RESULTS.read_text().splitlines():
            try:
                r = json.loads(line)
                keys.add((r["library"], r["benchmark"], r["budget"], r["seed"]))
            except (json.JSONDecodeError, KeyError):
                pass
    return keys


def cmd_for(arm, bench, budget, seed):
    if arm == "smac":
        return [str(SCRATCH / "smac_venv/bin/python"),
                str(HERE.parent / "bo_audit" / "smac_runner.py"),
                bench, str(budget), str(seed)]
    if arm == "optuna-tpe-3.6":
        return [str(SCRATCH / "optuna36_venv/bin/python"),
                str(HERE.parent / "bo_audit" / "optuna36_runner.py"),
                bench, str(budget), str(seed)]
    return [sys.executable, str(HERE / "g_cell_runner.py"), arm, bench,
            str(budget), str(seed)]


def one(job):
    arm, bench, budget, seed = job
    cap = cap_for(arm, bench, budget)
    t0 = time.time()
    try:
        p = subprocess.run(cmd_for(*job), capture_output=True, text=True,
                           timeout=cap, env=ENV)
    except subprocess.TimeoutExpired:
        with lock:
            FAILURES.open("a").write(f"TIMEOUT>{cap}s {arm} {bench} B{budget} {seed}\n")
        return f"TIMEOUT {arm} {bench} B{budget} {seed}"
    if p.returncode != 0:
        with lock:
            FAILURES.open("a").write(
                f"FAIL {arm} {bench} B{budget} {seed} :: {p.stderr.strip()[-400:]}\n\n")
        return f"FAIL {arm} {bench} B{budget} {seed}"
    row = json.loads(p.stdout.strip().splitlines()[-1])
    row.setdefault("wall_s", round(time.time() - t0, 1))
    row.update({"library": arm, "benchmark": bench, "budget": budget, "seed": seed})
    with lock:
        RESULTS.open("a").write(json.dumps(row) + "\n")
    return f"ok {arm} {bench} B{budget} {seed} ({row.get('wall_s')}s)"


def jobs_for(mode, classes):
    benches = [b for c in classes for b in CLASSES[c]]
    if mode == "smoke":
        return [("ax", "ml_rf_digits", 40, 4001),
                ("optuna-gp", "nk_n20k8", 80, 4001),
                ("skopt-gp", "labs_n25", 80, 4001),
                ("smac", "catf_michal_d5L9", 80, 4001),
                ("optuna-tpe-3.6", "yahpo_rpart_41138", 80, 4001)]
    out = []
    if mode == "fast":
        for arm in FAST_ARMS:
            for b in benches:
                if arm == "smac" and b not in SMAC_COVER:
                    continue
                for B in BUDGETS:
                    out += [(arm, b, B, s) for s in SEEDS]
    elif mode == "gp":
        for arm in GP_ARMS:
            for b in benches:
                for B in BUDGETS:
                    if B == 160 and b not in GP160:
                        continue
                    out += [(arm, b, B, s) for s in SEEDS]
    return out


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "smoke"
    classes = [c for c in sys.argv[2:] if c in CLASSES] or list("ABCDEF")
    jobs = jobs_for(mode, classes)
    done = done_keys()
    jobs = [j for j in jobs if j not in done]
    print(f"{mode} {classes}: {len(jobs)} runs to do ({len(done)} already done)")
    n = 0
    with ThreadPoolExecutor(max_workers=4) as ex:
        for msg in ex.map(one, jobs):
            n += 1
            if msg.startswith(("FAIL", "TIMEOUT")) or n % 100 == 0 or n == len(jobs):
                print(f"[{n}/{len(jobs)}] {msg}", flush=True)
    print("DONE")


if __name__ == "__main__":
    main()
