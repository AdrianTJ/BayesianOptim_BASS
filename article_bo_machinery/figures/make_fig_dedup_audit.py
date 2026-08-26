"""Regenerate figures/fig_dedup_audit.pdf from committed E2 results.

Cell: cat_ackley_d3_L5, keep generator, n_cand=1000 (exp02_oracle_matrix).
Matches the caption in main.tex: (a) oracle mean best-so-far, combination
vs encoding dedup, 25 paired seeds -- curves superimposed; (b) per-seed
revisit counts, median 78/80 under encoding dedup vs 0 under combination.
"""

import csv
import os

import matplotlib.pyplot as plt
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS = os.path.join(
    HERE, "..", "research", "article_loop", "experiments",
    "exp02_oracle_matrix", "results.csv",
)

BUDGETS = [10, 40, 80]


def load_cell():
    with open(RESULTS) as fh:
        rows = [
            r for r in csv.DictReader(fh)
            if r["objective"] == "cat_ackley_d3_L5"
            and r["generator"] == "keep"
            and r["n_cand"] == "1000"
        ]
    arms = {}
    for dedup in ("combination", "encoding"):
        sel = [r for r in rows if r["dedup"] == dedup]
        curves = np.array(
            [[float(r[f"best_b{b}"]) for b in BUDGETS] for r in sel]
        )
        revisits = np.array([int(r["revisits"]) for r in sel])
        arms[dedup] = {"curves": curves, "revisits": revisits}
    return arms


def main():
    arms = load_cell()
    plt.rcParams.update({"font.size": 8, "axes.linewidth": 0.6})

    fig, (ax_a, ax_b) = plt.subplots(
        1, 2, figsize=(3.4, 1.8), gridspec_kw={"wspace": 0.42}
    )

    # (a) Mean best-so-far: the two arms coincide because the oracle sits
    # at the optimum from the first checkpoint regardless of dedup level.
    comb_mean = arms["combination"]["curves"].mean(axis=0)
    enc_mean = arms["encoding"]["curves"].mean(axis=0)
    ax_a.plot(BUDGETS, comb_mean, "o-", color="black", lw=1.0,
              ms=3.5, label="combination")
    ax_a.plot(BUDGETS, enc_mean, "s--", color="black", lw=1.0,
              ms=3.5, mfc="white", label="encoding")
    ax_a.axhline(0.0, color="0.6", lw=0.5)
    ax_a.set_ylim(-0.03, 0.03)
    ax_a.annotate("both arms at the\noptimum ($4.4\\times10^{-16}$)",
                  xy=(40, 0.0), xytext=(38, 0.012),
                  ha="center", fontsize=7)
    ax_a.set_xlabel("budget $t$")
    ax_a.set_ylabel("oracle mean best-so-far")
    ax_a.legend(frameon=False, fontsize=7, loc="lower left")
    ax_a.text(0.02, 0.96, "(a)", transform=ax_a.transAxes,
              fontsize=8, va="top")

    # (b) Revisits: one dot per seed; thick bar = median.
    rng = np.random.default_rng(0)
    for i, dedup in enumerate(("combination", "encoding")):
        rev = arms[dedup]["revisits"]
        x = i + rng.uniform(-0.13, 0.13, size=rev.size)
        ax_b.scatter(x, rev, s=6, color="black", zorder=3,
                     clip_on=False)
        med = np.median(rev)
        ax_b.hlines(med, i - 0.28, i + 0.28, color="black", lw=3.0,
                    zorder=4)
        ax_b.annotate(f"{med:.0f}", xy=(i + 0.32, med),
                      va="center", fontsize=7)
    ax_b.set_xticks([0, 1], ["comb.", "enc."])
    ax_b.set_xlim(-0.55, 1.65)
    ax_b.set_ylim(0, 84)
    ax_b.set_ylabel("revisits per 80 evals")
    ax_b.text(0.02, 0.96, "(b)", transform=ax_b.transAxes,
              fontsize=8, va="top")

    out = os.path.join(HERE, "fig_dedup_audit.pdf")
    fig.savefig(out, bbox_inches="tight", pad_inches=0.02)
    print(f"wrote {out}")
    print("medians:", {d: float(np.median(a['revisits']))
                       for d, a in arms.items()})


if __name__ == "__main__":
    main()
