#!/usr/bin/env python3
"""H3 decay-law fit, per the pre-registered protocol in DESIGN.md.

Fit set: E8 medians (seeds 1026-1050). Held-out: E7 dial-arm medians
(seeds 1001-1025), scored with NO refitting. Prints markdown.
"""
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.optimize import least_squares
from scipy.stats import spearmanr

HERE = Path(__file__).resolve().parent
EXP = HERE.parents[1] / "article_loop" / "experiments"
SIGMAS = [0.0, 1.0, 3.0, 10.0, 30.0, 100.0]


def medians(df):
    out = {}
    for (obj, gen, nc), g in df.groupby(["objective", "generator", "n_cand"]):
        ms = g.groupby("sigma")["best_b80"].median()
        if set(SIGMAS) <= set(ms.index):
            out[(obj, gen, int(nc))] = np.array([ms[s] for s in SIGMAS])
    return out


def fit_cell(med):
    m0 = med[0]
    sig = np.array(SIGMAS[1:])
    y = med[1:]

    def resid(p):
        m_inf, log_s, log_g = p
        s, g = np.exp(log_s), np.exp(log_g)
        return m0 + (m_inf - m0) / (1 + (s / sig) ** g) - y

    best = None
    for s0 in (1.0, 10.0, 50.0):
        r = least_squares(resid, [y[-1], np.log(s0), 0.0], max_nfev=5000)
        if best is None or r.cost < best.cost:
            best = r
    m_inf, s, g = best.x[0], float(np.exp(best.x[1])), float(np.exp(best.x[2]))

    def curve(sg):
        sg = np.asarray(sg, dtype=float)
        out = m0 + (m_inf - m0) / (1 + (s / np.maximum(sg, 1e-12)) ** g)
        return np.where(sg == 0, m0, out)

    ss_res = float(np.sum((curve(sig) - y) ** 2))
    ss_tot = float(np.sum((y - y.mean()) ** 2))
    r2 = 1 - ss_res / ss_tot if ss_tot > 0 else float("nan")
    return curve, {"m0": m0, "m_inf": m_inf, "s": s, "gamma": g, "r2_fit": r2}


def main():
    e8 = pd.read_csv(EXP / "exp05_k10_final" / "results.csv")
    e7 = pd.read_csv(EXP / "exp04_guidance_dial" / "results.csv")
    e7 = e7[(e7.arm == "dial") & (e7.sigma.isin(SIGMAS)) & (e7.n_cand > 0)
            & (e7.generator.isin(["keep", "flip"]))]
    m8, m7 = medians(e8), medians(e7)

    print("# H3 decay-law fit (protocol: DESIGN.md)\n")
    print("| cell | m0 (σ=0) | m∞ (fit) | s (half-loss) | γ | R² fit (E8) | R² held-out (E7) | Spearman ρ(σ, median) E8 |")
    print("|---|---|---|---|---|---|---|---|")
    rows = []
    for key in sorted(m8):
        med8 = m8[key]
        curve, p = fit_cell(med8)
        rho = spearmanr(SIGMAS, med8).statistic
        r2h = float("nan")
        if key in m7:
            y7 = m7[key][1:]
            pred = curve(np.array(SIGMAS[1:]))
            sst = float(np.sum((y7 - y7.mean()) ** 2))
            r2h = 1 - float(np.sum((pred - y7) ** 2)) / sst if sst > 0 else float("nan")
        obj, gen, nc = key
        rows.append({**p, "cell": f"{obj}/{gen}/n{nc}", "rho": rho, "r2_held": r2h})
        print(f"| {obj}/{gen}/n{nc} | {p['m0']:.4f} | {p['m_inf']:.4f} | "
              f"{p['s']:.2f} | {p['gamma']:.2f} | {p['r2_fit']:.3f} | {r2h:.3f} | {rho:+.3f} |")

    mono = [r["cell"] for r in rows if r["rho"] < 1 - 1e-9 and r["rho"] > -1 + 1e-9
            and not np.isclose(abs(r["rho"]), 1.0, atol=1e-9)]
    # a cell is perfectly monotone (worsening) iff rho == +1 (higher sigma -> higher=worse median)
    non_mono = [r["cell"] for r in rows if not np.isclose(r["rho"], 1.0, atol=1e-9)]
    print(f"\nCells with NOT perfectly monotone-worsening medians (ρ<1): "
          f"{non_mono or 'none'} — disclosed per DESIGN, not smoothed.")
    print(f"\nMedian fitted half-loss dial s across cells: "
          f"{np.median([r['s'] for r in rows]):.1f} (per-cell values above).")


if __name__ == "__main__":
    main()
