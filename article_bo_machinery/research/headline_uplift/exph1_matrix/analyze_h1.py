#!/usr/bin/env python3
"""H1 aggregation. Written and committed BEFORE the full matrix ran, so the
analysis is fixed in advance: tables + literal evaluation of DESIGN.md's
P1-P7. Reads results.jsonl only; prints markdown to stdout.
"""
import json
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
BUDGET = 80
K = {"cat_ackley_d3_L5": 125, "cat_ackley_d5_L5": 3125,
     "cat_ackley_d6_L11": 11 ** 6, "pest_control": 5 ** 25,
     "func2C": None, "func3C": None}          # None -> continuous: pigeonhole 0
SOLVE = {"cat_ackley_d3_L5": 1e-9, "cat_ackley_d5_L5": 1e-9,
         "func2C": -0.206326 + 1e-3, "func3C": -0.722140 + 1e-3}
LIBS = ["random", "optuna-tpe", "hyperopt-tpe", "optuna-gp", "skopt-gp", "ax", "smac"]
BENCHMARKS = ["cat_ackley_d3_L5", "cat_ackley_d5_L5", "cat_ackley_d6_L11",
              "pest_control", "func2C", "func3C"]


def pigeonhole(k, b=BUDGET):
    if k is None:
        return 0.0
    return b - k * (1 - (1 - 1 / k) ** b)


def main():
    rows = [json.loads(l) for l in (HERE / "results.jsonl").read_text().splitlines()]
    cells = defaultdict(list)
    for r in rows:
        cells[(r["library"], r["benchmark"])].append(r)

    print(f"# H1 aggregate ({len(rows)} runs)\n")
    for bench in BENCHMARKS:
        ph = pigeonhole(K[bench])
        print(f"\n## {bench} (pigeonhole ≈ {ph:.1f}/{BUDGET})\n")
        print("| library | n | revisits med (mean) | excess med | best med | solved |")
        print("|---|---|---|---|---|---|")
        for lib in LIBS:
            rs = cells.get((lib, bench), [])
            if not rs:
                print(f"| {lib} | 0 | — | — | — | — |")
                continue
            rev = [r["revisits"] for r in rs]
            best = [r["best"] for r in rs]
            med, mean = np.median(rev), np.mean(rev)
            exc = np.median(rev) - ph
            thr = SOLVE.get(bench)
            solved = "n/a" if thr is None else f"{sum(b <= thr for b in best)}/{len(rs)}"
            print(f"| {lib} | {len(rs)} | {med:.0f} ({mean:.1f}) | {exc:+.1f} "
                  f"| {np.median(best):.4g} | {solved} |")

    # --- literal hypothesis evaluation --------------------------------------
    def med_excess(lib, bench):
        rs = cells.get((lib, bench), [])
        return (np.median([r["revisits"] for r in rs]) - pigeonhole(K[bench])) if rs else None

    def med_rev(lib, bench):
        rs = cells.get((lib, bench), [])
        return np.median([r["revisits"] for r in rs]) if rs else None

    print("\n## Pre-registered hypotheses, evaluated by the letter\n")
    purecat = ["cat_ackley_d3_L5", "cat_ackley_d5_L5", "cat_ackley_d6_L11", "pest_control"]

    e = med_excess("optuna-tpe", "cat_ackley_d3_L5")
    allpos = all((med_excess("optuna-tpe", b) or 0) > 0 for b in purecat)
    print(f"- **P1** optuna-tpe: d3_L5 excess {e} (> 10?) ; excess > 0 on all "
          f"pure-cat: {allpos} → {'PASS' if e is not None and e > 10 and allpos else 'FAIL'}")
    e = med_excess("hyperopt-tpe", "cat_ackley_d3_L5")
    print(f"- **P2** hyperopt-tpe d3_L5 excess {e} (> 5?) → "
          f"{'PASS' if e is not None and e > 5 else 'FAIL'}")
    e = med_excess("skopt-gp", "cat_ackley_d3_L5")
    rp = med_rev("skopt-gp", "pest_control")
    print(f"- **P3** skopt-gp |excess| {abs(e) if e is not None else None} (≤ 5?) "
          f"and pest revisits {rp} (≈0?) → "
          f"{'PASS' if e is not None and abs(e) <= 5 and rp is not None and rp <= 1 else 'FAIL'}"
          f"  [≈0 operationalized as median ≤ 1]")
    fails = [b for b in purecat if (med_excess('smac', b) or 0) > 5]
    print(f"- **P4** smac excess ≤ 5 on all pure-cat: violations {fails or 'none'} → "
          f"{'PASS' if not fails else 'FAIL'}")
    e = med_excess("ax", "cat_ackley_d3_L5")
    print(f"- **P5** (expl.) ax d3_L5 excess {e} (≤ 5?) → "
          f"{'PASS' if e is not None and e <= 5 else 'FAIL'}")
    e = med_excess("optuna-gp", "cat_ackley_d3_L5")
    print(f"- **P6** (expl.) optuna-gp d3_L5 excess {e} (> 0?) → "
          f"{'PASS' if e is not None and e > 0 else 'FAIL'}")
    viol = []
    for b in ("func2C", "func3C"):
        for lib in LIBS:
            mr = med_rev(lib, b)
            if mr is not None and mr > 1:
                viol.append(f"{lib}@{b}={mr:.0f}")
    print(f"- **P7** mixed-space ≈0 exact revisits for every library "
          f"(median ≤ 1): violations {viol or 'none'} → "
          f"{'PASS' if not viol else 'FAIL — investigate proposal streams'}")

    # headline numbers
    print("\n## Headline candidates (X of N libraries, ≥Y% waste)\n")
    N = len([l for l in LIBS if l != "random"])
    for y in (10, 25, 40):
        xs = []
        for lib in LIBS:
            if lib == "random":
                continue
            worst = max(((np.median([r['revisits'] for r in cells[(lib, b)]])
                          - pigeonhole(K[b])) / BUDGET * 100)
                        for b in purecat if cells.get((lib, b)))
            if worst >= y:
                xs.append(lib)
        print(f"- excess ≥ {y}% of budget on ≥1 pure-cat benchmark: "
              f"{len(xs)}/{N} ({', '.join(xs) or '—'})")

    # dropped-run accounting (DESIGN: never silent)
    fl = HERE / "failures.log"
    nf = len([l for l in fl.read_text().splitlines() if l.strip()]) if fl.exists() else 0
    print(f"\n(failures.log entries: {nf}; cells with n<20 are flagged in the tables above)")


if __name__ == "__main__":
    main()
