#!/usr/bin/env python3
"""H7 orchestrator: Ax with unordered categoricals, per DESIGN.md.

Uses the released package (top-level bo-audit/, driver fixed), not the
research copy under headline_uplift/bo_audit/, which stays as the record
of what H1 ran. Resumable: completed (benchmark, seed) keys in
results.jsonl are skipped. Each run is a subprocess with H1's caps;
failures land in failures.log with the stderr tail.

Usage: run_h7.py [python-with-ax]   (default: the current interpreter)
"""
import json
import os
import subprocess
import sys
import threading
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
PKG = HERE.parents[3] / "bo-audit"
RESULTS = HERE / "results.jsonl"
FAILURES = HERE / "failures.log"
PY = sys.argv[1] if len(sys.argv) > 1 else sys.executable

BENCHMARKS = ["cat_ackley_d3_L5", "cat_ackley_d5_L5", "cat_ackley_d6_L11",
              "pest_control", "func2C", "func3C"]
SEEDS = list(range(3001, 3026))
BUDGET = 80
CAP_S, CAP_PEST_S = 1200, 2700
WORKERS = 4
ENV = {**os.environ, "OMP_NUM_THREADS": "1", "MKL_NUM_THREADS": "1",
       "OPENBLAS_NUM_THREADS": "1", "PYTHONWARNINGS": "ignore"}

CELL = f"""
import json, sys, time, logging
logging.disable(logging.WARNING)
sys.path.insert(0, {str(PKG)!r})
from bo_audit.benchmarks import bench_by_name
from bo_audit.core import AuditedObjective
from bo_audit.drivers import run_ax
bench, seed = sys.argv[1], int(sys.argv[2])
fn, space = bench_by_name(bench)
audited = AuditedObjective(fn, space)
t0 = time.time()
cfg = run_ax(audited, space, {BUDGET}, seed)
out = audited.summary()
out.update(library="ax", benchmark=bench, seed=seed, budget={BUDGET},
           wall_s=round(time.time() - t0, 1), version=cfg["version"],
           non_defaults=cfg["non_defaults"])
print(json.dumps(out))
"""

lock = threading.Lock()


def done_keys():
    if not RESULTS.exists():
        return set()
    return {(r["benchmark"], r["seed"]) for r in map(json.loads, RESULTS.read_text().splitlines())}


def one(job):
    bench, seed = job
    cap = CAP_PEST_S if bench == "pest_control" else CAP_S
    try:
        out = subprocess.run([PY, "-c", CELL, bench, str(seed)], capture_output=True,
                             text=True, timeout=cap, env=ENV)
        line = out.stdout.strip().splitlines()[-1] if out.stdout.strip() else ""
        json.loads(line)
        with lock, RESULTS.open("a") as fh:
            fh.write(line + "\n")
    except Exception as exc:  # timeout, crash, unparsable output: record, never drop
        tail = getattr(locals().get("out"), "stderr", "")[-1500:]
        with lock, FAILURES.open("a") as fh:
            fh.write(f"{bench} {seed}: {exc!r}\n{tail}\n---\n")


def main():
    done = done_keys()
    jobs = [(b, s) for b in BENCHMARKS for s in SEEDS if (b, s) not in done]
    print(f"{len(jobs)} runs to go", flush=True)
    with ThreadPoolExecutor(WORKERS) as ex:
        list(ex.map(one, jobs))
    print(f"done: {len(done_keys())} results", flush=True)


if __name__ == "__main__":
    main()
