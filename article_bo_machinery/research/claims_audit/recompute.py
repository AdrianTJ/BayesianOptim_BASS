#!/usr/bin/env python3
"""Independent recomputation of the article's quantitative claims.

Reads only committed raw results (read-only) and prints, per claim, the
recomputed value next to what main.tex states. Needs numpy, scipy, pandas.

    python3 article_bo_machinery/research/claims_audit/recompute.py
"""
import itertools
import json
import random
import statistics as st
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import mannwhitneyu, wilcoxon

R = Path(__file__).resolve().parents[1]
HU = R / "headline_uplift"
EX = R / "article_loop" / "experiments"
FINAL = R.parents[1] / "final_results"


def jl(p):
    return [json.loads(line) for line in open(p)]


def section(title):
    print(f"\n=== {title}")


# --- C1: in-the-wild table (tab:wild) ---------------------------------------
section("C1 tab:wild (H1)")
h1 = jl(HU / "exph1_matrix" / "results.jsonl")
K = {"cat_ackley_d3_L5": 5 ** 3, "cat_ackley_d5_L5": 5 ** 5,
     "cat_ackley_d6_L11": 11 ** 6, "pest_control": 5 ** 25}


def pigeonhole(k, b):
    # expected repeats of B uniform draws over k cells; expm1/log1p for huge k
    return b - k * -np.expm1(b * np.log1p(-1.0 / k))


LIBS = ["random", "optuna-tpe", "hyperopt-tpe", "optuna-gp", "skopt-gp", "ax", "smac"]
for lib in LIBS:
    cells = []
    for b, k in K.items():
        rs = [r for r in h1 if r["library"] == lib and r["benchmark"] == b]
        med = st.median(r["revisits"] for r in rs)
        cells.append(f"{med:g}({med - pigeonhole(k, 80):+.0f})")
    sol = [sum(r["best"] < 1e-6 for r in h1 if r["library"] == lib and r["benchmark"] == b)
           for b in ("cat_ackley_d3_L5", "cat_ackley_d5_L5")]
    print(f"  {lib:13s} {' '.join(cells)}  solved d3={sol[0]}/25 d5={sol[1]}/25")
mixed = [r for r in h1 if r["benchmark"] in ("func2C", "func3C")]
print(f"  mixed-space runs with 0 revisits: {sum(r['revisits'] == 0 for r in mixed)}/{len(mixed)}")
print("  smac early terminations:", sorted(r["evals"] for r in h1 if r["library"] == "smac" and r["evals"] < 80))

# --- C2: equal-budget ranking change (Z=3/4) ---------------------------------
section("C2 ranking changes under memoization (H2) and their noise floor")
h2 = jl(HU / "exph2_control" / "results.jsonl")
RLIBS = ["optuna-tpe", "hyperopt-tpe", "optuna-gp", "skopt-gp", "ax", "smac"]


def vals(src, lib, b):
    return [r["best"] for r in sorted(src, key=lambda r: r["seed"])
            if r["library"] == lib and r["benchmark"] == b]


def key(v):  # pre-registered: median final best, solve-count tie-break
    return (round(st.median(v), 9), -sum(x < 1e-6 for x in v))


def order(keys):
    return {(a, c): (keys[a] > keys[c]) - (keys[a] < keys[c])
            for a, c in itertools.combinations(RLIBS, 2)}


random.seed(0)
for b in K:
    before = {lib: vals(h1, lib, b) for lib in RLIBS}
    after = {lib: (vals(h2, lib, b) or before[lib]) for lib in RLIBS}
    ob, oa = order({l: key(v) for l, v in before.items()}), order({l: key(v) for l, v in after.items()})
    flips = [(a, c, round(mannwhitneyu(after[a], after[c]).pvalue, 3))
             for (a, c) in ob if ob[(a, c)] != oa[(a, c)]]
    null = sum(order({l: key(random.choices(v, k=25)) for l, v in before.items()})
               != order({l: key(random.choices(v, k=25)) for l, v in before.items()})
               for _ in range(2000)) / 2000
    print(f"  {b:18s} changed={bool(flips)}  flipped pairs (post-memo MWU p)={flips}")
    print(f"  {'':18s} P(two bootstrap replicates of the SAME as-shipped data differ)={null:.2f}")
for lib, b in [("optuna-tpe", "cat_ackley_d5_L5"), ("hyperopt-tpe", "cat_ackley_d5_L5")]:
    a, s = vals(h2, lib, b), vals(h1, lib, b)
    print(f"  {lib} {b}: solves {sum(x < 1e-6 for x in s)} -> {sum(x < 1e-6 for x in a)}, "
          f"memo better on {sum(x < y - 1e-12 for x, y in zip(a, s))}/25, worse on {sum(x > y + 1e-12 for x, y in zip(a, s))}")

# --- C3: oracle-ceiling table and dedup ---------------------------------------
section("C3 tab:oracle and the 78/80 dedup figure (E2)")
e2 = pd.read_csv(EX / "exp02_oracle_matrix" / "results.csv")
opt = {"func2C": -0.206326, "func3C": -0.722140}
c = e2[e2.dedup == "combination"]
for (o, n), g in c[c.objective.isin(opt)].groupby(["objective", "n_cand"]):
    k = g[g.generator == "keep"].sort_values("seed").best_b80.values
    f = g[g.generator == "flip"].sort_values("seed").best_b80.values
    print(f"  {o} N={n:4d} perm {k.mean():.4f} restr {f.mean():.4f} gap {(f - k).mean():.4f} "
          f"wins {(f > k).sum()}/25 p={wilcoxon(f, k).pvalue:.2g} max residual {(k - opt[o]).max():.1e}")
ca = e2[(e2.objective == "cat_ackley_d3_L5") & (e2.generator == "keep")]
print("  oracle Cat-Ackley d3: solved by eval 10 on",
      int((ca[ca.dedup == "encoding"].best_b10 < 1e-6).sum()), "/25 seeds; encoding-dedup median revisits",
      ca[ca.dedup == "encoding"].revisits.median())

# --- C4: surrogate matrix dedup + oracle domination ---------------------------
section("C4 tab:dedup and the 0-of-2,250 domination record (E3 vs E2)")
e3 = pd.read_csv(EX / "exp03_surrogate_matrix" / "results.csv")
cat = e3[e3.objective == "cat_ackley_d3_L5"]
for s in ("gp", "rf"):
    for gen in ("keep", "flip"):
        g = cat[(cat.surrogate == s) & (cat.generator == gen)]
        a = g[g.dedup == "combination"].sort_values("seed").best_b80.values
        e = g[g.dedup == "encoding"].sort_values("seed").best_b80.values
        nz = (np.abs(a - e) > 1e-9).sum()
        p = wilcoxon(a, e).pvalue if nz else 1.0
        print(f"  {s} {gen}: encoding solved {(e < 0.1).sum()}/25, worse than combination on {(e > a + 1e-9).sum()} seeds, p={p:.3g}")
o1000 = e2[e2.n_cand == 1000]
strong = o1000[(o1000.generator == "keep") & (o1000.dedup == "combination")].set_index(["objective", "seed"])
n = v = 0
for _, r in e3.iterrows():
    for b in ("best_b10", "best_b40", "best_b80"):
        n += 1
        v += r[b] < strong.loc[(r.objective, r.seed)][b] - 1e-12
print(f"  as stated (all E3 arms incl. TPE/Random vs permissive+combination oracle): {v}/{n} violations")
viol = []
for _, r in e3[e3.surrogate.isin(["gp", "rf"])].iterrows():
    ref = o1000[(o1000.objective == r.objective) & (o1000.seed == r.seed)
                & (o1000.generator == r.generator) & (o1000.dedup == r.dedup)].iloc[0]
    for b in ("best_b10", "best_b40", "best_b80"):
        if r[b] < ref[b] - 1e-12:
            viol.append((r.objective, r.seed, r.surrogate, r.generator, r.dedup, b, round(r[b], 5), round(ref[b], 5)))
print(f"  like-for-like (GP/RF vs oracle under the SAME generator+dedup): {len(viol)}/1800 violations")
for x in viol:
    print("   ", x)

# --- C5: guidance dial retest at the anchors ----------------------------------
section("C5 guidance-dial retest (E8) and anchor calibration")
e8 = pd.read_csv(EX / "exp05_k10_final" / "results.csv")
for (o, nc, sg), g in e8.groupby(["objective", "n_cand", "sigma"]):
    k = g[g.generator == "keep"].sort_values("seed").best_b80.values
    f = g[g.generator == "flip"].sort_values("seed").best_b80.values
    p = wilcoxon(k, f).pvalue if np.any(k != f) else 1.0
    tag = " <- anchor" if (o, sg) in {("func2C", 10.0), ("func3C", 30.0)} else ""
    print(f"  {o} N={nc:4d} sigma={sg:5.0f} keep wins {(k < f).sum():2d}/25 p={p:.3g} keep median {np.median(k):+.3f}{tag}")
for o in ("func2C", "func3C"):
    g = e3[(e3.objective == o) & (e3.dedup == "combination")]
    print(f"  {o} E3 medians: GP keep {g[(g.surrogate == 'gp') & (g.generator == 'keep')].best_b80.median():+.3f}, "
          f"RF keep {g[(g.surrogate == 'rf') & (g.generator == 'keep')].best_b80.median():+.3f}, "
          f"Random {e3[(e3.objective == o) & (e3.surrogate == 'random')].best_b80.median():+.3f}")

# --- C6: thesis-pipeline paired table -----------------------------------------
section("C6 tab:thesis and the NLP task (final_results)")
for b in ("func2C", "func3C", "cat_ackley_d3_L5", "nlp_hpo"):
    a = pd.read_csv(FINAL / b / "all_runs.csv")
    f = a[a.iter == a.iter.max()].pivot(index="seed", columns="method", values="best")
    for m in [x for x in f.columns if x != "Random"]:
        d = f[m] - f["Random"]
        w, l = int((d < -1e-12).sum()), int((d > 1e-12).sum())
        nz = d[abs(d) > 1e-12]
        print(f"  {b:16s} {m:8s} {w}/{len(d) - w - l}/{l}  exact Wilcoxon p={wilcoxon(nz).pvalue:.3g}")
