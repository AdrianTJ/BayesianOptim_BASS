#!/usr/bin/env python3
"""H1 orchestrator. Protocol per DESIGN.md (commit DESIGN before full run).

Resumable: completed (library, benchmark, seed) keys in results.jsonl are
skipped, so the fan-out can be re-launched across loop cycles. Each run is
its own subprocess with a hard 20-min cap; kills/failures land in
failures.log with the tail of stderr (never silent).

Usage:
  run_h1.py smoke   # 1 seed per (slow-lib x benchmark) to validate the cap
  run_h1.py full    # the whole pre-registered matrix
"""
import json
import subprocess
import sys
import threading
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
RESULTS = HERE / "results.jsonl"
FAILURES = HERE / "failures.log"
SMAC_VENV_PY = Path("/tmp/claude-0/-home-user-BayesianOptim-BASS/"
                    "d8cc14fb-4c54-5880-818f-3a67a8836a1b/scratchpad/smac_venv/bin/python")

LIBS = ["random", "optuna-tpe", "hyperopt-tpe", "optuna-gp", "skopt-gp", "ax", "smac"]
BENCHMARKS = ["cat_ackley_d3_L5", "cat_ackley_d5_L5", "cat_ackley_d6_L11",
              "pest_control", "func2C", "func3C"]
SEEDS = list(range(3001, 3026))
BUDGET = 80
CAP_S = 1200
CAP_PEST_S = 2700   # amended before the full run: smoke showed GP-family
                    # libraries need >20 min on the 25-dim pest space
WORKERS = 4
# 4 workers on 4 cores: pin library thread pools so runs don't oversubscribe
import os
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
    import time
    lib, bench, seed = job
    cap = CAP_PEST_S if bench == "pest_control" else CAP_S
    if lib == "smac":
        cmd = [str(SMAC_VENV_PY), str(HERE.parent / "bo_audit" / "smac_runner.py"),
               bench, str(BUDGET), str(seed)]
    else:
        cmd = [sys.executable, str(HERE / "cell_runner.py"), lib, bench,
               str(BUDGET), str(seed)]
    t0 = time.time()
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=cap,
                           env=ENV)
    except subprocess.TimeoutExpired:
        with lock:
            FAILURES.open("a").write(f"TIMEOUT>{cap}s {lib} {bench} {seed}\n")
        return f"TIMEOUT {lib} {bench} {seed}"
    if p.returncode != 0:
        with lock:
            FAILURES.open("a").write(
                f"FAIL {lib} {bench} {seed} :: {p.stderr.strip()[-500:]}\n\n")
        return f"FAIL {lib} {bench} {seed}"
    row = json.loads(p.stdout.strip().splitlines()[-1])
    row.setdefault("wall_s", round(time.time() - t0, 1))
    row.update({"library": lib, "benchmark": bench, "seed": seed})
    with lock:
        RESULTS.open("a").write(json.dumps(row) + "\n")
    return f"ok {lib} {bench} {seed} ({row.get('wall_s', '?')}s)"


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "full"
    if mode == "smoke":
        jobs = [(lib, bench, 3001) for lib in ("ax", "optuna-gp", "skopt-gp", "smac")
                for bench in BENCHMARKS]
    else:
        jobs = [(lib, bench, seed) for lib in LIBS for bench in BENCHMARKS
                for seed in SEEDS]
    done = done_keys()
    jobs = [j for j in jobs if j not in done]
    print(f"{mode}: {len(jobs)} runs to do ({len(done)} already in results.jsonl)")
    n = 0
    with ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for msg in ex.map(one, jobs):
            n += 1
            print(f"[{n}/{len(jobs)}] {msg}", flush=True)
    print("DONE")


if __name__ == "__main__":
    main()
