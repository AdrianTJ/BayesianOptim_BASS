#!/usr/bin/env python3
"""G-sweep aggregation — committed BEFORE wave 1 (DESIGN procedure step 3).
Reads results.jsonl; prints markdown: per-class e(B) tables, per-arm
budget curves, solve rates against g0 ground truth, and the letter
evaluation of GH1–GH7. No number reaches the paper except through here
(extended h_numbers pipeline follows in G5)."""
import json
import math
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np
from scipy.stats import spearmanr, kendalltau

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
from run_g import CLASSES, FAST_ARMS, GP_ARMS, SMAC_COVER, O36_SKIP, BUDGETS, GP160

ARMS = FAST_ARMS + GP_ARMS
NODEDUP = ["optuna-tpe", "optuna-tpe-3.6", "hyperopt-tpe", "optuna-gp"]
K = {  # combination counts for pure-cat spaces; None => has float dims => pigeonhole 0
    "cat_ackley_d3_L5": 125, "cat_ackley_d5_L5": 3125, "cat_ackley_d6_L11": 11**6,
    "catf_rastrigin_d4L7": 7**4, "catf_griewank_d5L7": 7**5,
    "catf_rosen_d4L7": 7**4, "catf_michal_d5L9": 9**5, "catf_schwefel_d4L9": 9**4,
    "nk_n20k2": 2**20, "nk_n20k8": 2**20, "maxcut_n20": 2**20, "labs_n25": 2**25,
    "pest_control": 5**25, "contam_2p25": 2**25,
    "ml_rf_digits": 4*2*19*28,   # cat x cat x int x int — finite (no floats)
    "ml_svm_digits": None, "ml_gb_bc": None, "ml_mlp_wine": None,
    "yahpo_rpart_41138": None, "yahpo_rpart_40981": None, "yahpo_ranger_1489": None,
    "func2C": None, "func3C": None,
}
GT = json.loads((HERE.parent / "h6_generalize" / "g0_ground_truth.json").read_text())["ground_truth"]
SOLVE = {b: v["min"] + 1e-9 for b, v in GT.items()}
SOLVE.update({"cat_ackley_d3_L5": 1e-9, "cat_ackley_d5_L5": 1e-9,
              "func2C": -0.206326 + 1e-3, "func3C": -0.722140 + 1e-3})
FLOATY = {b for b, k in K.items() if k is None}


def pigeonhole(k, b):
    if k is None:
        return 0.0
    return max(0.0, b + k * math.expm1(b * math.log1p(-1.0 / k)))


def covered(arm, bench):
    if arm == "smac" and bench not in SMAC_COVER:
        return False
    if arm == "optuna-tpe-3.6" and bench in O36_SKIP:
        return False
    return True


def main():
    rows = [json.loads(l) for l in (HERE / "results.jsonl").read_text().splitlines()]
    cells = defaultdict(list)
    for r in rows:
        cells[(r["library"], r["benchmark"], r["budget"])].append(r)

    def med_e(arm, bench, B):
        rs = cells.get((arm, bench, B), [])
        if not rs:
            return None
        ph = pigeonhole(K[bench], B)
        return float(np.median([(r["revisits"] - ph) / B for r in rs]))

    def med_rev(arm, bench, B):
        rs = cells.get((arm, bench, B), [])
        return float(np.median([r["revisits"] for r in rs])) if rs else None

    def med_best(arm, bench, B):
        rs = cells.get((arm, bench, B), [])
        return float(np.median([r["best"] for r in rs])) if rs else None

    def solves(arm, bench, B):
        thr = SOLVE.get(bench)
        rs = cells.get((arm, bench, B), [])
        return None if thr is None or not rs else sum(r["best"] <= thr for r in rs)

    print(f"# G-sweep aggregate ({len(rows)} runs)\n")

    # per-class e(80) table
    print("## Median e(80) per arm x class (per-class median of per-benchmark medians)\n")
    hdr = "| arm | " + " | ".join("ABCDEF") + " |"
    print(hdr); print("|" + "---|" * 7)
    class_med = {}
    for arm in ARMS:
        vals = []
        for c in "ABCDEF":
            es = [med_e(arm, b, 80) for b in CLASSES[c]
                  if covered(arm, b) and med_e(arm, b, 80) is not None]
            m = float(np.median(es)) if es else None
            class_med[(arm, c)] = m
            vals.append("—" if m is None else f"{m:+.3f}")
        print(f"| {arm} | " + " | ".join(vals) + " |")

    # per-benchmark full table
    print("\n## Median e(B) per arm x benchmark x budget\n")
    print("| benchmark | arm | " + " | ".join(f"B{B}" for B in BUDGETS) +
          " | solves@80 | med best@80 |")
    print("|" + "---|" * (4 + len(BUDGETS)))
    for c in "ABCDEF":
        for b in CLASSES[c]:
            for arm in ARMS:
                if not covered(arm, b):
                    continue
                es = ["—" if med_e(arm, b, B) is None else f"{med_e(arm, b, B):+.3f}"
                      for B in BUDGETS]
                sv = solves(arm, b, 80)
                mb = med_best(arm, b, 80)
                print(f"| {b} | {arm} | " + " | ".join(es) +
                      f" | {sv if sv is not None else 'n/a'} | "
                      f"{'—' if mb is None else f'{mb:.4g}'} |")

    # ---- hypotheses, by the letter -----------------------------------------
    print("\n## Pre-registered hypotheses (letter evaluation)\n")

    def gh1_for(arm):
        ok = [c for c in "ABCDE"
              if class_med.get((arm, c)) is not None and class_med[(arm, c)] > 0.05]
        return ok

    for tag, arm in (("GH1a", "optuna-tpe"), ("GH1b", "hyperopt-tpe")):
        ok = gh1_for(arm)
        print(f"- **{tag}** {arm} e(80)>0.05 per class: {ok} "
              f"({len(ok)}/5) → {'PASS' if len(ok) >= 4 else 'FAIL'}")

    viol = []
    for arm in ("ax", "smac"):
        for c in "ABCDEF":
            for b in CLASSES[c]:
                if not covered(arm, b):
                    continue
                for B in BUDGETS:
                    if (arm in GP_ARMS and B == 160 and b not in GP160):
                        continue
                    mr = med_rev(arm, b, B)
                    if mr is not None and mr > 0:
                        viol.append(f"{arm}@{b}/B{B}={mr:.0f}")
    sk = []
    for c in "ABCDEF":
        for b in CLASSES[c]:
            for B in BUDGETS:
                if B == 160 and b not in GP160:
                    continue
                e = med_e("skopt-gp", b, B)
                if e is not None and abs(e) > 0.07:
                    sk.append(f"skopt@{b}/B{B}={e:+.3f}")
    print(f"- **GH2** ax/smac zero-revisit violations: {viol or 'none'}; "
          f"skopt |e|>0.07: {sk or 'none'} → {'PASS' if not viol and not sk else 'FAIL'}")

    tot = pos = 0
    for arm in NODEDUP:
        for c in "ABCDEF":
            for b in CLASSES[c]:
                if not covered(arm, b):
                    continue
                if arm == "optuna-gp" and b not in GP160:
                    es = [med_e(arm, b, B) for B in BUDGETS[:3]]
                    Bs = BUDGETS[:3]
                else:
                    es = [med_e(arm, b, B) for B in BUDGETS]
                    Bs = BUDGETS
                if any(e is None for e in es):
                    continue
                tot += 1
                rho = spearmanr(Bs, es).statistic
                if not np.isnan(rho) and rho >= 0:
                    pos += 1
                elif np.isnan(rho):        # constant e(B) counts as non-decreasing
                    pos += 1
    frac = pos / tot if tot else float("nan")
    print(f"- **GH3** rho(B, e)>=0 in {pos}/{tot} = {frac:.2f} of no-dedup cells "
          f"→ {'PASS' if tot and frac >= 0.70 else 'FAIL'}")

    ok36 = gh1_for("optuna-tpe-3.6")
    print(f"- **GH4** optuna-tpe-3.6 classes passing: {ok36} ({len(ok36)}/5) "
          f"→ {'PASS' if len(ok36) >= 4 else 'FAIL'} "
          f"(class E over ml_* only, per Amendment 1)")

    gh5 = {}
    for arm in NODEDUP:
        n = sum(1 for b in CLASSES["E"]
                if covered(arm, b) and (med_e(arm, b, 80) or 0) >= 0.05)
        gh5[arm] = n
    best_arm = max(gh5, key=gh5.get)
    print(f"- **GH5** real-ML benchmarks with e(80)>=0.05 per no-dedup arm: {gh5} "
          f"→ {'PASS' if gh5[best_arm] >= 4 else 'FAIL'}")

    print("- **GH6** (descriptive) Kendall tau between B=20 and B=160 arm rankings "
          "(median best; fast arms, all covered benchmarks):")
    for c in "ABCDEF":
        taus = []
        for b in CLASSES[c]:
            arms = [a for a in FAST_ARMS if covered(a, b)]
            r20 = [med_best(a, b, 20) for a in arms]
            r160 = [med_best(a, b, 160) for a in arms]
            if any(v is None for v in r20 + r160):
                continue
            taus.append(kendalltau(r20, r160).statistic)
        if taus:
            print(f"    class {c}: median tau {np.median(taus):+.2f} over {len(taus)} benchmarks")

    gh7 = []
    for b in FLOATY:
        mr = med_rev("random", b, 80)
        if mr is not None and mr > 0:
            gh7.append(f"random@{b}={mr:.0f}")
    nz = []
    for arm in NODEDUP:
        for b in FLOATY:
            if covered(arm, b):
                mr = med_rev(arm, b, 80)
                if mr is not None and mr > 0:
                    nz.append(f"{arm}@{b}={mr:.0f}")
    print(f"- **GH7** random median revisits on float-bearing spaces: violations "
          f"{gh7 or 'none'} → {'PASS' if not gh7 else 'FAIL'}; no-dedup nonzero "
          f"(findings, not failures): {nz or 'none'}")

    fl = HERE / "failures.log"
    nf = (len([l for l in fl.read_text().splitlines()
               if l.startswith(("FAIL", "TIMEOUT"))]) if fl.exists() else 0)
    print(f"\n(distinct failed run attempts: {nf}; incomplete cells flagged as — above)")


if __name__ == "__main__":
    main()
