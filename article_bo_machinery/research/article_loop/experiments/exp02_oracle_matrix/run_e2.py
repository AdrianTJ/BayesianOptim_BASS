#!/usr/bin/env python3
"""E2 oracle-ceiling matrix. Protocol per DESIGN.md.

Part A: 4 benchmarks x {keep,flip} x {combination,encoding} + random,
        n_cand=1000, 25 seeds, budget 80, revisit instrumentation.
Part B: func2C/func3C x {keep,flip} x n_cand {50,200}, combination dedup.
Writes results.csv and prints H1-H5 checks.
"""
import csv
import sys
from pathlib import Path

import numpy as np
from scipy.stats import sem

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from machinery import (Config, RANDOM, make_cat_ackley, make_func2C,
                       make_func3C, oracle_method, run_bo, shared_init)

SEEDS = list(range(1001, 1026))
BENCH = {
    "func2C": make_func2C,
    "func3C": make_func3C,
    "cat_ackley_d3_L5": lambda: make_cat_ackley(3, 5),
    "cat_ackley_d6_L11": lambda: make_cat_ackley(6, 11),
}
rows = []

def one(obj, seed, X0, y0, gen, ded, n_cand, arm_ix):
    cfg = Config(budget=80, n_cand=n_cand)
    m = RANDOM if gen == "random" else oracle_method(obj, cfg, gen)
    rng = np.random.default_rng(seed * 100 + arm_ix)
    r = run_bo(obj, m, cfg, X0, y0, rng, dedup=ded)
    b = r["best"]
    rows.append({"objective": obj["name"], "seed": seed, "generator": gen,
                 "dedup": ded, "n_cand": n_cand, "best_b10": b[10],
                 "best_b40": b[40], "best_b80": b[80], "revisits": r["revisits"]})

for bname, make in BENCH.items():
    obj = make()
    for seed in SEEDS:
        X0, y0 = shared_init(obj, seed)
        arm_ix = 0
        # Part A cells
        for gen in ("keep", "flip"):
            for ded in ("combination", "encoding"):
                one(obj, seed, X0, y0, gen, ded, 1000, arm_ix); arm_ix += 1
        one(obj, seed, X0, y0, "random", "combination", 1000, arm_ix); arm_ix += 1
        # Part B pool axis (mixed benchmarks only)
        if bname in ("func2C", "func3C"):
            for gen in ("keep", "flip"):
                for nc in (50, 200):
                    one(obj, seed, X0, y0, gen, "combination", nc, arm_ix); arm_ix += 1
    print(bname, "done", flush=True)

out = Path(__file__).parent / "results.csv"
with out.open("w", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=list(rows[0]))
    w.writeheader(); w.writerows(rows)

# ---- hypothesis checks ------------------------------------------------------
def sel(**kw):
    col = kw.pop("col", "best_b80")
    v = [r[col] for r in rows if all(r[k] == kv for k, kv in kw.items())]
    return np.array(v)

def report(label, ok, detail):
    print(f"[{'PASS' if ok else 'FAIL'}] {label}: {detail}"); return ok

ok = True
# H1: direction at 25 seeds
for o in ("func2C", "func3C"):
    k = sel(objective=o, generator="keep", dedup="combination", n_cand=1000)
    f = sel(objective=o, generator="flip", dedup="combination", n_cand=1000)
    w = int(np.sum(k < f))
    ok &= report(f"H1 {o} keep wins", w >= 20, f"{w}/25 paired wins; means keep {k.mean():.4f} flip {f.mean():.4f}")

# H2: gap widens as pool shrinks
h2_detail, h2_ok = [], True
for o in ("func2C", "func3C"):
    gaps = {}
    for nc in (50, 200, 1000):
        k = sel(objective=o, generator="keep", dedup="combination", n_cand=nc)
        f = sel(objective=o, generator="flip", dedup="combination", n_cand=nc)
        gaps[nc] = (f - k).mean()
    h2_ok &= gaps[50] > gaps[200] > gaps[1000]
    h2_detail.append(f"{o} gap(50/200/1000) = {gaps[50]:.4f}/{gaps[200]:.4f}/{gaps[1000]:.4f}")
ok &= report("H2 ceiling gap grows as pool shrinks", h2_ok, "; ".join(h2_detail))

# H3: dedup invisible in final value
h3_ok, h3_d = True, []
for o in BENCH:
    for gen in ("keep", "flip"):
        c = sel(objective=o, generator=gen, dedup="combination", n_cand=1000)
        e = sel(objective=o, generator=gen, dedup="encoding", n_cand=1000)
        diff, s = abs(c.mean() - e.mean()), sem(c - e)
        if s > 0 and diff >= 2 * s:
            h3_ok = False
            h3_d.append(f"{o}/{gen}: diff {diff:.4g} > 2*SE {2*s:.4g}")
ok &= report("H3 dedup leak invisible in convergence", h3_ok, "; ".join(h3_d) or "all cells within 2 SE")

# H4: encoding dedup devours budget on d3L5
enc = sel(objective="cat_ackley_d3_L5", generator="keep", dedup="encoding", n_cand=1000, col="revisits")
com = sel(objective="cat_ackley_d3_L5", generator="keep", dedup="combination", n_cand=1000, col="revisits")
ok &= report("H4 silent budget loss", np.median(enc) >= 40 and np.median(com) == 0,
             f"median revisits/80: encoding {np.median(enc):.0f}, combination {np.median(com):.0f}")

# H5: pure-cat ceilings generator-independent
h5_ok, h5_d = True, []
for o in ("cat_ackley_d3_L5", "cat_ackley_d6_L11"):
    for gen in ("keep", "flip"):
        n = int(np.sum(sel(objective=o, generator=gen, dedup="combination", n_cand=1000) < 0.1))
        h5_ok &= n >= 23
        h5_d.append(f"{o}/{gen} {n}/25")
ok &= report("H5 pure-cat ceilings clear", h5_ok, "; ".join(h5_d))

print("\nE2 OVERALL:", "PASS" if ok else "SEE FAILURES ABOVE")
