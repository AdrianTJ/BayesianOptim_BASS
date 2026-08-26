#!/usr/bin/env python3
"""H2 aggregation — written and committed BEFORE the full run. Computes the
pre-registered Z-of-W ranking-change number and evaluates Q1/Q2 by the
letter of DESIGN.md. Reads this dir's results.jsonl (H2, memoized) and
../exph1_matrix/results.jsonl (H1, as-shipped, same seeds)."""
import json
from collections import defaultdict
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
BENCHMARKS = ["cat_ackley_d3_L5", "cat_ackley_d5_L5", "cat_ackley_d6_L11", "pest_control"]
AUDITED = ["optuna-tpe", "hyperopt-tpe", "optuna-gp", "skopt-gp", "ax", "smac"]
RERUN = {"optuna-tpe": BENCHMARKS,
         "optuna-gp": BENCHMARKS[:3],   # pest carries over (0 H1 revisits)
         "hyperopt-tpe": BENCHMARKS}
SOLVE = {"cat_ackley_d3_L5": 1e-9, "cat_ackley_d5_L5": 1e-9}
EXACT0 = 1e-9
TIE = 1e-9


def load(path):
    cells = defaultdict(list)
    for line in Path(path).read_text().splitlines():
        r = json.loads(line)
        cells[(r["library"], r["benchmark"])].append(r)
    return cells


def med_best(cells, lib, bench):
    rs = cells.get((lib, bench), [])
    return float(np.median([r["best"] for r in rs])) if rs else None


def solves(cells, lib, bench):
    thr = SOLVE.get(bench)
    rs = cells.get((lib, bench), [])
    return None if thr is None or not rs else sum(r["best"] <= thr for r in rs)


def ranking_pairs(vals, solve_counts=None):
    """Strictly-ordered pairs (a beats b) per DESIGN's registered metric:
    median best (TIE tolerance), tie-break by solve count where a solve
    threshold exists. Amendment 2: the first committed version omitted the
    tie-break clause that DESIGN itself registers — found by adversarial
    review (it hid the d3_L5 solve-count ranking change); fixed post-data
    TOWARD the registered definition, disclosed in ANALYSIS/REVIEW."""
    pairs = set()
    for a in vals:
        for b in vals:
            if a == b or vals[a] is None or vals[b] is None:
                continue
            if vals[a] < vals[b] - TIE:
                pairs.add((a, b))
            elif abs(vals[a] - vals[b]) <= TIE and solve_counts is not None:
                sa, sb = solve_counts.get(a), solve_counts.get(b)
                if sa is not None and sb is not None and sa > sb:
                    pairs.add((a, b))
    return pairs


def main():
    h1 = load(HERE.parent / "exph1_matrix" / "results.jsonl")
    h2 = load(HERE / "results.jsonl")

    def h2_or_h1(lib, bench):
        return h2 if bench in RERUN.get(lib, []) else h1

    print("# H2 aggregate\n")
    z = 0
    flips_all = {}
    for bench in BENCHMARKS:
        vals1 = {lib: med_best(h1, lib, bench) for lib in AUDITED}
        vals2 = {lib: med_best(h2_or_h1(lib, bench), lib, bench) for lib in AUDITED}
        sc1 = {lib: solves(h1, lib, bench) for lib in AUDITED} \
            if bench in SOLVE else None
        sc2 = {lib: solves(h2_or_h1(lib, bench), lib, bench) for lib in AUDITED} \
            if bench in SOLVE else None
        p1, p2 = ranking_pairs(vals1, sc1), ranking_pairs(vals2, sc2)
        flips = p1.symmetric_difference(p2)
        changed = bool(flips)
        z += changed
        flips_all[bench] = sorted(flips)
        print(f"\n## {bench} — ranking {'CHANGED' if changed else 'unchanged'}\n")
        print("| library | H1 as-shipped med best (solve) | H2 equalized med best (solve) | n H2 | uniq/prop (H2 med) |")
        print("|---|---|---|---|---|")
        for lib in AUDITED:
            s1, s2 = solves(h1, lib, bench), solves(h2_or_h1(lib, bench), lib, bench)
            rs2 = h2.get((lib, bench), [])
            up = (f"{np.median([r['unique_evals_charged'] for r in rs2]):.0f}/"
                  f"{np.median([r['proposals'] for r in rs2]):.0f}" if rs2 else "carried")
            f1 = "—" if vals1[lib] is None else f"{vals1[lib]:.4g}"
            f2 = "—" if vals2[lib] is None else f"{vals2[lib]:.4g}"
            print(f"| {lib} | {f1} ({s1 if s1 is not None else 'n/a'}) "
                  f"| {f2} ({s2 if s2 is not None else 'n/a'}) | {len(rs2)} | {up} |")
        if flips:
            print(f"\nOrder flips: {sorted(flips)}")

    print(f"\n## Z-of-W\n\n**Z = {z} of W = 4** benchmarks change ranking "
          f"once budgets are equalized (pairwise strict-order flips, tie tol {TIE}).")
    print(f"\n- **Q1** (Z ≥ 1): {'PASS' if z >= 1 else 'FAIL — reported as the honest null'}")

    # Q2 by the letter
    viol, strict_results = [], []
    for lib, benches in RERUN.items():
        for bench in benches:
            m1, m2 = med_best(h1, lib, bench), med_best(h2, lib, bench)
            if m2 is not None and m1 is not None and m2 > m1 + 1e-12:
                viol.append(f"{lib}@{bench}: H2 {m2:.6g} worse than H1 {m1:.6g}")
    strict_cells = [("optuna-tpe", "cat_ackley_d5_L5"), ("optuna-tpe", "cat_ackley_d6_L11"),
                    ("optuna-tpe", "pest_control"), ("hyperopt-tpe", "cat_ackley_d5_L5")]
    for lib, bench in strict_cells:
        m1, m2 = med_best(h1, lib, bench), med_best(h2, lib, bench)
        ok = m1 is not None and m2 is not None and m2 < m1 - TIE
        strict_results.append(f"{lib}@{bench}: {m1:.4g} -> {m2:.4g} {'STRICT-IMPROVED' if ok else 'NOT strict'}")
    solve_cells = [("optuna-tpe", "cat_ackley_d3_L5", 21), ("hyperopt-tpe", "cat_ackley_d3_L5", 21)]
    for lib, bench, base in solve_cells:
        s2 = solves(h2, lib, bench)
        strict_results.append(f"{lib}@{bench} solves: {base}/25 -> {s2}/25 "
                              f"{'ROSE' if s2 is not None and s2 > base else 'did NOT rise'}")
    og_stay = [("optuna-gp", b) for b in ("cat_ackley_d3_L5", "cat_ackley_d5_L5")]
    for lib, bench in og_stay:
        m2 = med_best(h2, lib, bench)
        strict_results.append(f"{lib}@{bench}: stays at exact optimum: "
                              f"{'YES' if m2 is not None and m2 <= EXACT0 else 'NO'}")
    print(f"\n- **Q2** weak-improvement violations (harness-bug tripwire): {viol or 'none'}")
    for s in strict_results:
        print(f"  - {s}")

    fl = HERE / "failures.log"
    nf = (len([l for l in fl.read_text().splitlines()
               if l.startswith(("FAIL", "TIMEOUT"))]) if fl.exists() else 0)
    print(f"\n(distinct failed run attempts: {nf})")


if __name__ == "__main__":
    main()
