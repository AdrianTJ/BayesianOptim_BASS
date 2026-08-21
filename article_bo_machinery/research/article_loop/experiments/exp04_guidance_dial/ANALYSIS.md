# E7 — Analysis

**Date:** 2026-08-22 · **Protocol:** as DESIGN.md (frozen before the run).
1250 runs. Raw: `results.csv`. Pre-registered composite verdict:
**INCONCLUSIVE** — reported as such; the decomposition below explains
exactly which clause failed and why, without altering the verdict.

## The dial table (W = paired keep<flip wins/25; G = mean flip−keep gap)

| cell | σ=0 | σ=1 | σ=3 | σ=10 | σ=30 | σ=100 |
|---|---|---|---|---|---|---|
| func2C n=1000 | **W25** p6e-8 | W23 p2e-5 | W18 p.004 | W13 p.38 | W12 p.58 | W13 p.22 |
| func2C n=50 | **W25** p6e-8 | W21 p4e-4 | W15 p.25 | W10 p.91 | W13 p.43 | W16 p.14 |
| func3C n=1000 | **W25** p6e-8 | W25 p6e-8 | W16 p.60 | W14 p.60 | W15 p.38 | W9 p.36 |
| func3C n=50 | **W25** p6e-8 | W22 p6e-5 | W16 p.26 | W16 p.10 | W14 p.81 | W14 p.33 |

## Decision-rule decomposition (pre-registered clauses)

- **W-clauses: all four cells consistent with H-regime.** W(0) = 25/25
  everywhere (replicates E2 within this experiment); at the pre-named
  GP-level anchors σ*, W ∈ {10,13,14,15} with p ∈ [0.10, 0.91] — sensitivity
  gone precisely where guidance reaches surrogate-level quality, within the
  identical pool-argmax search type. The refute criterion (effect persisting
  at σ*) is cleanly not met.
- **G-clause failed → composite INCONCLUSIVE.** The clause required
  G(σ†=100) < G(0) in all four cells; it fails in **two of four** (both
  func2C pools: e.g. n=1000 G(0)=+0.000818 vs G(100)=+0.032335) — the
  first draft of this document said "three", a counting error caught by
  review. The two func3C cells "pass" only via noise-driven movements (a
  sign flip to −0.0198 at n=1000; a modest shrink at n=50) — consistent
  with the diagnosis that G at σ† is noise-dominated, but the honest
  statement is: the G statistic behaved erratically in all four cells, in
  two of them in the direction that fails the clause. Post-hoc inspection
  suggests the clause was mis-specified at design time: at near-ceiling
  cells both generators converge (E2's own finding — flip reaches −0.2051
  at n=1000), so G(0) is structurally near zero and cannot reliably exceed
  noise-inflated gaps at σ†. This diagnosis is post-hoc and therefore does
  NOT upgrade the verdict.

**Status of K10:** remains a hypothesis. The de-confounding the E3 review
demanded has now been *performed* and the guidance-degradation direction
came out as H-regime predicts on the (cleaner) win-rate criterion in 4/4
cells — but by our own pre-registration the composite is inconclusive. If
the hypothesis is to enter the article as supported, it needs one more
pre-registered test with win-rate-based criteria fixed in advance (cheap:
the same dial, fresh seeds 1026–1050, criteria on W only). Otherwise the
article uses only the E3 fallback and may describe the dial as "suggestive,
inconclusive under our pre-registered composite" with the table shown.

## Shared-init TPE (K-TPE)

| benchmark | stock TPE (E3) | shared-init TPE | pre-reg threshold ≥17 |
|---|---|---|---|
| func2C | 15/25, mean −0.0581 | **20/25**, mean −0.1128 | closes |
| func3C | 12/25, mean −0.2253 | 16/25, mean −0.2733 | misses by one seed |

**Pre-registered outcome: the joint criterion (≥17/25 on BOTH benchmarks)
is NOT met** — func3C is 16/25 — so by DESIGN.md's own binary rule the
recorded result is the fallback branch: **discrepancy narrowed but not
resolved; K-TPE stays open.** (The first draft of this document declared
K-TPE "closed" with a qualifier; the independent review correctly flagged
that as an unauthorized post-hoc softening of a pre-registered rule — the
same failure mode E3's review blocked — and it is retracted.)

What can be said within the pre-registration: injecting the shared LHS
init moved TPE from Random-indistinguishable (15/25, 12/25) to 20/25 with
clearly better means on func2C, and to 16/25 (means −0.273 vs −0.178) on
func3C. The init-machinery hypothesis is strongly indicated but not
confirmed to criterion; a follow-up (more seeds, or optuna sampler-config
matching) is required before any article claim rests on it.

Two configuration deviations from the thesis's `tpe.R`, disclosed per
review (both absent from the first-draft DESIGN): our shared-init arm sets
`n_startup_trials=0`, whereas `tpe.R` keeps optuna's default (10) — for
func2C (n0=9) that makes our first post-init trial TPE-guided where the
thesis's would still be random, a small pro-TPE asymmetry (1 of 80
trials); and the noise rng for the dial omits the benchmark index, making
noise streams identical across benchmarks at matching (seed, arm) — a
cosmetic non-independence between cells that no reported statistic relies
on.

## Threats to validity

- The verdict-relevant caveat from DESIGN stands: i.i.d. noise is one
  degradation model; biased surrogates (systematic over-smoothing) are
  uncovered.
- W at σ=100 sits slightly above 12.5/25 in 3/4 cells (13,16,14) — tiny
  residual signal or noise; the pre-registered rules made no claim there.
- G-clause critique above is post-hoc; any re-test must be freshly seeded
  and pre-registered.

## Ledger impact (post-review)

- K10 → stays hypothesis; evidence note updated (W-clauses consistent with
  H-regime in all four cells under the de-confounded dial; composite
  INCONCLUSIVE by pre-registration; a one-shot, fresh-seed, W-only re-test
  is specified as the *final* test — its result stands for the article
  either way, no further re-tests).
- K-TPE → **stays open** (pre-registered joint criterion missed: func3C
  16/25). Status narrowed: init machinery is the strongly-indicated
  dominant cause; follow-up required before any article use.
- Review verdict: REFUTED on the first draft's K-TPE closure and a G-count
  factual error; both corrected here (see REVIEW.md).
