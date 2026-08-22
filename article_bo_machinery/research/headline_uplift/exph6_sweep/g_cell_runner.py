#!/usr/bin/env python3
"""Run ONE G-sweep cell run (main-env arms) and print a JSON line.
Usage: g_cell_runner.py <arm> <benchmark> <budget> <seed>
Venv arms (smac, optuna-tpe-3.6) do not come here — run_g.py routes them
to their runners."""
import json
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))                                     # bo_audit
_MACH = HERE.parents[1] / "article_loop" / "experiments"                 # machinery
assert (_MACH / "machinery.py").exists(), f"machinery path wrong: {_MACH}"
sys.path.insert(0, str(_MACH))


def main():
    arm, bench, budget, seed = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
    from bo_audit.benchmarks import bench_by_name
    from bo_audit.core import AuditedObjective
    from bo_audit.drivers import DRIVERS

    fn, space = bench_by_name(bench)
    audited = AuditedObjective(fn, space,
                               canonicalize=getattr(fn, "_canonicalize", None))
    t0 = time.time()
    cfg = DRIVERS[arm](audited, space, budget, seed)
    out = audited.summary()
    out.update({"library": arm, "benchmark": bench, "seed": seed,
                "budget": budget, "wall_s": round(time.time() - t0, 1),
                "version": cfg.get("version"), "non_defaults": cfg.get("non_defaults")})
    print(json.dumps(out))


if __name__ == "__main__":
    main()
