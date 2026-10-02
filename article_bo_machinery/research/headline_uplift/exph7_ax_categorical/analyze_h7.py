#!/usr/bin/env python3
"""H7 aggregation: nominal Ax (this dir) against the committed ordinal Ax
rows in H1, by the letter of DESIGN.md (X1-X3 plus descriptive outputs).
Reads results.jsonl here, ../exph1_matrix/results.jsonl and
../exph2_control/results.jsonl; prints markdown to stdout."""
import json
import math
from collections import defaultdict
from pathlib import Path

import numpy as np
from scipy.stats import wilcoxon

HERE = Path(__file__).resolve().parent
BUDGET = 80
BENCHMARKS = ["cat_ackley_d3_L5", "cat_ackley_d5_L5", "cat_ackley_d6_L11",
              "pest_control", "func2C", "func3C"]
K = {"cat_ackley_d3_L5": 125, "cat_ackley_d5_L5": 3125,
     "cat_ackley_d6_L11": 11 ** 6, "pest_control": 5 ** 25,
     "func2C": None, "func3C": None}
SOLVE = 1e-6                                  # DESIGN: < 1e-6 on Cat-Ackley
CATACK = BENCHMARKS[:3]
TIE = 1e-9


def pigeonhole(k, b=BUDGET):
    if k is None:
        return 0.0
    return max(0.0, b + k * math.expm1(b * math.log1p(-1.0 / k)))


def load(path):
    cells = defaultdict(dict)
    for line in Path(path).read_text().splitlines():
        r = json.loads(line)
        cells[(r["library"], r["benchmark"])][r["seed"]] = r
    return cells


def solves(rs, bench):
    return sum(r["best"] < SOLVE for r in rs) if bench in CATACK else None


def rank_order(a, b, bench):
    """H2 metric: median final best (TIE), solve-count tie-break."""
    ma, mb = np.median([r["best"] for r in a]), np.median([r["best"] for r in b])
    if ma < mb - TIE:
        return "first"
    if mb < ma - TIE:
        return "second"
    sa, sb = solves(a, bench), solves(b, bench)
    if sa is not None and sa != sb:
        return "first" if sa > sb else "second"
    return "tie"


def main():
    h7 = load(HERE / "results.jsonl")
    h1 = load(HERE.parent / "exph1_matrix" / "results.jsonl")
    h2 = load(HERE.parent / "exph2_control" / "results.jsonl")

    print("# H7 aggregation\n")
    print("| benchmark | n | revisits med | excess med | solves nominal | solves ordinal (H1) "
          "| best med nominal | best med ordinal | nominal better/worse/equal | Wilcoxon p |")
    print("|---|---|---|---|---|---|---|---|---|---|")
    total_rev = 0
    n_total = 0
    for b in BENCHMARKS:
        nom, ordi = h7[("ax", b)], h1[("ax", b)]
        seeds = sorted(set(nom) & set(ordi))
        rv = [nom[s]["revisits"] for s in nom]
        total_rev += sum(rv)
        n_total += len(nom)
        x = np.array([nom[s]["best"] for s in seeds])
        y = np.array([ordi[s]["best"] for s in seeds])
        d = x - y
        better = int((d < -TIE).sum()); worse = int((d > TIE).sum())
        equal = len(d) - better - worse
        try:
            p = f"{wilcoxon(x, y).pvalue:.3g}" if better + worse else "n/a"
        except ValueError:
            p = "n/a"
        sn, so = solves(nom.values(), b), solves(ordi.values(), b)
        print(f"| {b} | {len(nom)} | {np.median(rv):g} | {np.median(rv) - pigeonhole(K[b]):.2f} "
              f"| {'n/a' if sn is None else f'{sn}/{len(nom)}'} "
              f"| {'n/a' if so is None else f'{so}/{len(ordi)}'} "
              f"| {np.median(x):.4g} | {np.median(y):.4g} | {better}/{worse}/{equal} | {p} |")

    print(f"\n## X1 (0 revisits in all 150 runs)\n\nruns: {n_total}, total revisits: "
          f"{total_rev} -> **{'PASS' if n_total == 150 and total_rev == 0 else 'FAIL'}**")

    b = "cat_ackley_d5_L5"
    sn = solves(h7[("ax", b)].values(), b)
    print(f"\n## X2 (d5 solves < 18/25 ordinal)\n\nnominal {sn}/25 vs committed ordinal "
          f"{solves(h1[('ax', b)].values(), b)}/25 -> **{'PASS' if sn < 18 else 'FAIL'}**")

    print("\n## X3 (H2 headline pair on cat_ackley_d5_L5, descriptive)\n")
    nom = list(h7[("ax", b)].values())
    for label, cells in (("optuna-tpe memoized (H2)", h2), ("optuna-tpe as shipped (H1)", h1)):
        tpe = list(cells[("optuna-tpe", b)].values())
        o = rank_order(tpe, nom, b)
        who = {"first": "optuna-tpe ahead", "second": "nominal Ax ahead", "tie": "tie"}[o]
        print(f"- {label}: median best {np.median([r['best'] for r in tpe]):.4g}, "
              f"solves {solves(tpe, b)}/25; nominal Ax median best "
              f"{np.median([r['best'] for r in nom]):.4g}, solves {sn}/25 -> {who}")


if __name__ == "__main__":
    main()
