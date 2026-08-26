# H3 — Analysis (decay-law fit; theory in THEORY.md)

**Date:** 2026-08-21 · **Protocol:** DESIGN.md, committed before the fit
ran. Fit set E8 medians; held-out E7 medians, no refitting.

## Reading (mixed result, reported as measured)

- **func3C (all 4 cells): the decay law works.** In-sample R² 0.89–1.00,
  held-out R² 0.66–0.88 on disjoint seeds with no refitting, empirical
  medians perfectly monotone-worsening in σ in 3 of 4 cells (ρ=+1.0;
  the fourth ρ=+0.94). Fitted half-loss dial s: 3.2–22.3 **in raw f-units** (σ is additive
  noise SD on the acquisition score; no normalized "objective scale"
  is defined, so s is reported in raw units only — review finding) —
  larger pools and the keep generator tolerate more guidance noise
  before losing half the oracle's edge, consistent with K10's direction.
- **func2C (all 4 cells): the fit is underdetermined and does NOT
  replicate cleanly.** The benchmark sits near its optimum across most of
  the σ grid (median spread ~0.2 total), so three parameters chase tiny
  differences: two cells fit with γ≈18 (step-function artifacts) and
  held-out R² collapses (0.12, 0.31, 0.63, and **−1.28** in
  func2C/keep/n1000 — worse than predicting E7's mean). We report this
  as a *failure of the fitted law to transfer on func2C*, attributable to
  range compression, and do not use func2C fitted parameters anywhere.
- **Monotonicity disclosure:** 5 of 8 cells have ρ < 1 (smallest +0.77):
  small non-monotone wiggles in empirical medians exist. Proposition
  C(a) predicts monotone *single-draw selection probability*, not
  monotone *sequential-loop medians* (THEORY.md C(c) draws this boundary
  explicitly); the wiggles are within seed noise for 25-seed medians and
  are disclosed, not smoothed.

## What H3 adds to the paper

1. **Lemma A** upgrades the audit from "diagnostic yardstick" to
   **theorem in the exogenous-pool case**, with the exact boundary
   (adaptive generators) stated as Proposition B, an explicit conjecture
   with its empirical record (0 violations across E2/E3 matched cells)
   and an explicit counterexample construction (purely-local generator).
2. **Proposition C** gives the guidance dial a law: monotone selection
   decay with pinned limits (oracle → uniform-over-pool), Gaussian case
   proved, Gumbel/softmax closed-form twin stated.
3. **The fitted half-loss s** is the quantitative bridge from K10's
   qualitative decay: on func3C, half the oracle's edge survives up to
   dial noise of ~3–22× the objective's scale depending on
   generator/pool — with the func2C non-transfer honestly on record.

## Deviations & threats

- Anchor papers remain uncited (egress; per gate). Author to-do queued.
- 5 positive-σ points vs 3 free parameters: in-sample R² is nearly
  meaningless alone; the held-out column is the substantive evidence,
  and it splits by benchmark as above.
- Median-of-25 curves; no per-seed fits.
- The σ→∞ plateau m∞ is extrapolated (largest σ run: 100).

## Ledger impact (pending prover-skeptic review)

Proposed: **H3-LEMA** (theorem + boundary), **H3-PROPC** (proved
monotone decay + closed-form twin; fit transfers on func3C, fails to
transfer on func2C — both in the claim), **H3-ANCHORS** (open author
to-do, not a claim).

---

## Fit output (fit_propc.py)

# H3 decay-law fit (protocol: DESIGN.md)

| cell | m0 (σ=0) | m∞ (fit) | s (half-loss) | γ | R² fit (E8) | R² held-out (E7) | Spearman ρ(σ, median) E8 |
|---|---|---|---|---|---|---|---|
| func2C/flip/n50 | -0.1919 | -0.0106 | 1.12 | 0.68 | 0.758 | 0.117 | +0.771 |
| func2C/flip/n1000 | -0.2053 | -0.0178 | 1.29 | 1.64 | 0.988 | 0.307 | +0.771 |
| func2C/keep/n50 | -0.2063 | -0.0455 | 3.09 | 18.33 | 0.621 | 0.630 | +0.943 |
| func2C/keep/n1000 | -0.2063 | -0.0033 | 3.40 | 18.37 | 0.998 | -1.275 | +0.943 |
| func3C/flip/n50 | -0.6651 | -0.0937 | 4.85 | 0.89 | 0.977 | 0.810 | +1.000 |
| func3C/flip/n1000 | -0.7154 | -0.1527 | 3.23 | 1.22 | 0.891 | 0.878 | +0.943 |
| func3C/keep/n50 | -0.7221 | -0.3077 | 7.80 | 3.32 | 1.000 | 0.658 | +1.000 |
| func3C/keep/n1000 | -0.7221 | -0.0256 | 22.25 | 1.76 | 1.000 | 0.863 | +1.000 |

Cells with NOT perfectly monotone-worsening medians (ρ<1): ['func2C/flip/n50', 'func2C/flip/n1000', 'func2C/keep/n50', 'func2C/keep/n1000', 'func3C/flip/n1000'] — disclosed per DESIGN, not smoothed.

Median fitted half-loss dial s across cells: 3.3 (per-cell values above).
