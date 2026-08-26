#!/usr/bin/env python3
"""E1 harness validation: reproduce the R oracle-ceiling findings.

Protocol per DESIGN.md: func2C, func3C, cat_ackley d6 L11; arms
oracle+keep / oracle+flip / random on shared per-seed initial designs;
15 seeds (1001-1015), budget 80, n_cand 1000. Writes results.csv (per-seed
best-so-far at budgets 10/40/80) and prints the V1-V6 checks.
"""
import csv
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from machinery import (Config, RANDOM, make_cat_ackley, make_func2C,
                       make_func3C, oracle_method, run_bo, shared_init)

SEEDS = range(1001, 1016)
cfg = Config(budget=80, n_cand=1000)

rows = []
for make in (make_func2C, make_func3C, lambda: make_cat_ackley(6, 11)):
    obj = make()
    for seed in SEEDS:
        X0, y0 = shared_init(obj, seed)
        for arm_ix, (arm_name, method) in enumerate((
            ("oracle_keep", oracle_method(obj, cfg, "keep")),
            ("oracle_flip", oracle_method(obj, cfg, "flip")),
            ("random", RANDOM),
        )):
            # explicit arm index: deterministic across processes and collision-
            # free between arms (review finding: hash() is salted per process)
            rng = np.random.default_rng(seed * 10 + arm_ix)
            res = run_bo(obj, method, cfg, X0, y0, rng)
            b = res["best"]
            rows.append({"objective": obj["name"], "seed": seed, "arm": arm_name,
                         "best_b10": b[10], "best_b40": b[40], "best_b80": b[80]})
        print(f"{obj['name']} seed {seed} done", flush=True)

out = Path(__file__).parent / "results.csv"
with out.open("w", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=list(rows[0]))
    w.writeheader()
    w.writerows(rows)

# ---- V-checks ---------------------------------------------------------------
def arm(objname, armname, col):
    return np.array([r[col] for r in rows
                     if r["objective"] == objname and r["arm"] == armname])

def report(label, ok, detail):
    print(f"[{'PASS' if ok else 'FAIL'}] {label}: {detail}")
    return ok

k2, f2 = arm("func2C", "oracle_keep", "best_b80"), arm("func2C", "oracle_flip", "best_b80")
k2_10, f2_10 = arm("func2C", "oracle_keep", "best_b10"), arm("func2C", "oracle_flip", "best_b10")
k3, f3 = arm("func3C", "oracle_keep", "best_b80"), arm("func3C", "oracle_flip", "best_b80")
ca_k = arm("cat_ackley_d6_L11", "oracle_keep", "best_b80")
ca_f = arm("cat_ackley_d6_L11", "oracle_flip", "best_b80")

ok = True
ok &= report("V1 func2C keep hits optimum",
             abs(k2.mean() - (-0.2063)) < 1e-3 and k2_10.mean() < -0.20,
             f"mean final {k2.mean():.4f} (target -0.2063); mean best@10 {k2_10.mean():.4f}")
ok &= report("V2 func2C flip early plateau",
             -0.19 <= f2_10.mean() <= -0.11,
             f"mean best@10 {f2_10.mean():.4f} (R: ~ -0.148)")
wins2 = int(np.sum(k2 < f2))
ok &= report("V3 func2C paired wins keep>flip", wins2 >= 12,
             f"{wins2}/15 (R: 15/15); mean flip final {f2.mean():.4f}")
wins3 = int(np.sum(k3 < f3))
ok &= report("V4 func3C keep vs flip",
             abs(k3.mean() - (-0.7216)) < 2e-3 and -0.72 <= f3.mean() <= -0.67 and wins3 >= 12,
             f"keep {k3.mean():.4f} (target -0.7216), flip {f3.mean():.4f} (R: ~ -0.697), wins {wins3}/15")
n_ok = int(np.sum(ca_k < 0.1)), int(np.sum(ca_f < 0.1))
ok &= report("V5 cat_ackley both arms clear pool",
             n_ok[0] >= 13 and n_ok[1] >= 13,
             f"keep {n_ok[0]}/15, flip {n_ok[1]}/15 below 0.1 (means {ca_k.mean():.3f}/{ca_f.mean():.3f})")
r_worse = all(arm(o, "random", "best_b80").mean() > max(arm(o, "oracle_keep", "best_b80").mean(),
                                                        arm(o, "oracle_flip", "best_b80").mean())
              for o in ("func2C", "func3C", "cat_ackley_d6_L11"))
ok &= report("V6 random sanity", r_worse, "random mean final worse than oracle arms on all benchmarks")

print("\nE1 OVERALL:", "PASS" if ok else "FAIL")
sys.exit(0 if ok else 1)
