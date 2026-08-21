#!/usr/bin/env python3
"""Run ONE H2 memoized run (library, benchmark, seed); print a JSON line."""
import json
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))                                     # bo_audit
sys.path.insert(0, str(HERE.parents[2] / "article_loop" / "experiments"))  # machinery
sys.path.insert(0, str(HERE))                                            # memo_drivers


def main():
    lib, bench, budget, seed = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
    from bo_audit.benchmarks import bench_by_name
    from bo_audit.memo import MemoizedAuditedObjective
    from memo_drivers import MEMO_DRIVERS

    fn, space = bench_by_name(bench)
    audited = MemoizedAuditedObjective(fn, space, unique_budget=budget)
    t0 = time.time()
    cfg = MEMO_DRIVERS[lib](audited, space, budget, seed)
    out = audited.summary()
    out.update({"library": lib, "benchmark": bench, "seed": seed,
                "budget": budget, "wall_s": round(time.time() - t0, 1),
                "version": cfg.get("version"), "non_defaults": cfg.get("non_defaults")})
    print(json.dumps(out))


if __name__ == "__main__":
    main()
