#!/usr/bin/env python3
"""H2 orchestrator (protocol per DESIGN.md). Resumable like run_h1.py."""
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

CELLS = ([("optuna-tpe", b) for b in ("cat_ackley_d3_L5", "cat_ackley_d5_L5",
                                      "cat_ackley_d6_L11", "pest_control")]
         + [("optuna-gp", b) for b in ("cat_ackley_d3_L5", "cat_ackley_d5_L5",
                                       "cat_ackley_d6_L11")]
         + [("hyperopt-tpe", b) for b in ("cat_ackley_d3_L5", "cat_ackley_d5_L5",
                                          "cat_ackley_d6_L11", "pest_control")])
SEEDS = list(range(3001, 3026))
BUDGET = 80
CAP_S = 2700
WORKERS = 4
ENV = {**os.environ, "OMP_NUM_THREADS": "1", "MKL_NUM_THREADS": "1",
       "OPENBLAS_NUM_THREADS": "1"}
lock = threading.Lock()


def done_keys():
    keys = set()
    if RESULTS.exists():
        for line in RESULTS.read_text().splitlines():
            try:
                r = json.loads(line)
                keys.add((r["library"], r["benchmark"], r["seed"]))
            except (json.JSONDecodeError, KeyError):
                pass
    return keys


def one(job):
    lib, bench, seed = job
    cmd = [sys.executable, str(HERE / "h2_cell_runner.py"), lib, bench,
           str(BUDGET), str(seed)]
    t0 = time.time()
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=CAP_S, env=ENV)
    except subprocess.TimeoutExpired:
        with lock:
            FAILURES.open("a").write(f"TIMEOUT>{CAP_S}s {lib} {bench} {seed}\n")
        return f"TIMEOUT {lib} {bench} {seed}"
    if p.returncode != 0:
        with lock:
            FAILURES.open("a").write(
                f"FAIL {lib} {bench} {seed} :: {p.stderr.strip()[-500:]}\n\n")
        return f"FAIL {lib} {bench} {seed}"
    row = json.loads(p.stdout.strip().splitlines()[-1])
    row.setdefault("wall_s", round(time.time() - t0, 1))
    with lock:
        RESULTS.open("a").write(json.dumps(row) + "\n")
    return f"ok {lib} {bench} {seed} ({row.get('wall_s')}s, {row.get('unique_evals_charged')}u/{row.get('proposals')}p)"


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "full"
    if mode == "smoke":
        jobs = [("optuna-gp", "cat_ackley_d3_L5", 3001),
                ("optuna-gp", "cat_ackley_d6_L11", 3001),
                ("optuna-tpe", "pest_control", 3001),
                ("hyperopt-tpe", "cat_ackley_d5_L5", 3001)]
    else:
        jobs = [(lib, bench, seed) for lib, bench in CELLS for seed in SEEDS]
    done = done_keys()
    jobs = [j for j in jobs if j not in done]
    print(f"{mode}: {len(jobs)} runs to do ({len(done)} already done)")
    n = 0
    with ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for msg in ex.map(one, jobs):
            n += 1
            print(f"[{n}/{len(jobs)}] {msg}", flush=True)
    print("DONE")


if __name__ == "__main__":
    main()
