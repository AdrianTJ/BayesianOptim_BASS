#!/usr/bin/env python3
"""H0 instrumentation validation. Protocol per DESIGN.md."""
import json
import sys
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))                      # bo_audit
sys.path.insert(0, str(HERE.parents[1] / "article_loop" / "experiments"))  # machinery
from bo_audit.core import AuditedObjective
from bo_audit.drivers import DRIVERS
from machinery import make_cat_ackley

results = {}

def report(label, ok, detail):
    print(f"[{'PASS' if ok else 'FAIL'}] {label}: {detail}")
    results[label] = {"pass": bool(ok), "detail": detail}
    return ok

# --- benchmark adapter -------------------------------------------------------
obj_ca = make_cat_ackley(3, 5)
SPACE_CA = [(f"x{j}", "cat", list(range(1, 6))) for j in range(3)]

def cat_ackley_cfg(cfg):
    u = np.array([[(cfg[f"x{j}"] - 0.5) / 5 for j in range(3)]])
    return float(obj_ca["fn"](u)[0])

ok = True

# G1: scripted repeats
a = AuditedObjective(lambda c: 0.0, [("a", "cat", [1, 2, 3, 4, 5])])
for _ in range(7):
    a({"a": 1})
for v in (2, 3, 4):
    a({"a": v})
ok &= report("G1 scripted repeats", a.n_revisits == 6 and len(a.seen) == 4,
             f"revisits {a.n_revisits} (want 6), unique {len(a.seen)} (want 4)")

# G2: continuous random, no false positives
g2_ok = True
for seed in range(5):
    sp = [(f"c{j}", "float", 0.0, 1.0) for j in range(4)]
    a = AuditedObjective(lambda c: sum(c.values()), sp)
    DRIVERS["random"](a, sp, 200, seed)
    g2_ok &= a.n_revisits == 0
ok &= report("G2 continuous no-false-positives", g2_ok, "0 revisits in all 5 runs" if g2_ok else "false positive found")

# G3 + G4: tiny categorical pigeonhole + independent recount
sp3 = [("a", "cat", [0, 1]), ("b", "cat", [0, 1])]
revs, recount_ok = [], True
for seed in range(20):
    a = AuditedObjective(lambda c: 0.0, sp3)
    DRIVERS["random"](a, sp3, 40, seed)
    revs.append(a.n_revisits)
    # G4 brute recount from the raw call log
    seen, n = set(), 0
    for key, _ in a.calls:
        if key in seen:
            n += 1
        seen.add(key)
    recount_ok &= (n == a.n_revisits)
exp_g3 = 40 - 4 * (1 - (3 / 4) ** 40)
ok &= report("G3 pigeonhole expectation", abs(np.mean(revs) - exp_g3) <= 2,
             f"mean revisits {np.mean(revs):.1f}, analytic ≈ {exp_g3:.1f}")
ok &= report("G4 independent recount", recount_ok, "exact match all 20 runs")

# D1-D4: documented behavior on Cat-Ackley d3 L5, budget 80, seeds 2001-2010
SEEDS = range(2001, 2011)
per_lib = {}
for lib in ("hyperopt-tpe", "optuna-tpe", "skopt-gp", "random"):
    rows = []
    for seed in SEEDS:
        a = AuditedObjective(cat_ackley_cfg, SPACE_CA)
        DRIVERS[lib](a, SPACE_CA, 80, seed)
        s = a.summary()
        rows.append(s)
    per_lib[lib] = rows
    med = float(np.median([r["revisits"] for r in rows]))
    mean = float(np.mean([r["revisits"] for r in rows]))
    best = float(np.mean([r["best"] for r in rows]))
    print(f"  {lib}: revisits median {med:.0f} mean {mean:.1f} /80 | mean best {best:.3f}")

d1 = float(np.median([r["revisits"] for r in per_lib["hyperopt-tpe"]]))
ok &= report("D1 hyperopt detection", d1 >= 5, f"median revisits {d1:.0f}/80 (gate ≥5; issue #608 class)")
d2 = float(np.median([r["revisits"] for r in per_lib["optuna-tpe"]]))
ok &= report("D2 optuna detection", d2 > 0, f"median revisits {d2:.0f}/80 (gate >0; issues #5440/#2021 class)")
d3 = float(np.mean([r["revisits"] for r in per_lib["skopt-gp"]]))
print(f"[INFO] D3 skopt-gp descriptive: mean revisits {d3:.1f}/80")
d4 = float(np.mean([r["revisits"] for r in per_lib["random"]]))
exp_d4 = 80 - 125 * (1 - (124 / 125) ** 80)
ok &= report("D4 random pigeonhole on benchmark", abs(d4 - exp_d4) <= 5,
             f"mean {d4:.1f}, analytic ≈ {exp_d4:.1f}")

json.dump({"checks": results, "per_lib": per_lib},
          open(HERE / "results.json", "w"), indent=1)
print("\nH0 GATE:", "OPEN (instrumentation validated)" if ok else "CLOSED — FIX BEFORE H1")
sys.exit(0 if ok else 1)
