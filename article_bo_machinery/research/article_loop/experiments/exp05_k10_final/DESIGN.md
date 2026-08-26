# E8 — K10 final test (design, written before running)

**Date:** 2026-08-22 · **Purpose:** The single authorized re-test of the
K10 hypothesis (CLAIMS.md; E7 ANALYSIS): does keep-vs-flip generator
sensitivity decay with guidance quality within the identical pool-argmax
search type? E7's dial showed exactly this pattern on its win-rate clauses
but returned INCONCLUSIVE on a mis-specified gap clause. This is a fresh-
seed, win-rate-only re-run. **Its outcome is final for the article: no
further K10 experiments regardless of result.**

## Protocol

Identical to E7's dial except: **fresh seeds 1026–1050** (never used in any
prior experiment), and the noise rng now folds in the benchmark index
(fixing E7's disclosed cross-benchmark noise-stream reuse). Arms: {keep,
flip} × σ ∈ {0, 1, 3, 10, 30, 100} × n_cand ∈ {1000, 50} × {func2C,
func3C}; combination dedup; budget 80; 1200 runs. Anchors carried over
from E7's frozen design (chosen from the pilot, before any confirmatory
data): σ* = 10 (func2C), 30 (func3C); the full σ ladder is retained only
for the descriptive decay curve.

## Pre-registered decision rules (win-rate only; evaluated by the letter)

Per cell (benchmark × pool), W(σ) = paired keep<flip wins/25, p = Wilcoxon.

| Outcome | Criterion (quoted verbatim from the Cycle-4 authorization) |
|---|---|
| **H-regime SUPPORTED** | W(0) ≥ 20 & p < 0.05 AND W(σ*) ≤ 17 & p ≥ 0.05, in all four cells |
| **H-regime REFUTED** | W(σ*) ≥ 20 & p < 0.05 at n_cand=50 on both benchmarks |
| **INCONCLUSIVE-FINAL** | anything else |

Ledger consequences, fixed now: SUPPORTED → K10 becomes a supported claim
citing E8 (the article may present the regime diagnostic, with E7's
composite honestly footnoted). REFUTED → K10 is closed-refuted; the
article states the generator effect as oracle-specific. INCONCLUSIVE-FINAL
→ K10 is dropped from the article entirely; only the E3 fallback framing
is used. No re-litigation in any branch.

## Threats

- Same degradation-model limitation as E7 (i.i.d. noise ≠ biased
  surrogate); stated in the article regardless of outcome.
- This is the second look at the same hypothesis family; the fresh seeds
  and letter-first evaluation are the multiplicity control, and the
  no-third-test rule is the stopping control.
