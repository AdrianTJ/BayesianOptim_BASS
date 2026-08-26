# E8 — Analysis (K10 final test)

**Date:** 2026-08-22 · **Protocol:** as DESIGN.md; fresh seeds 1026–1050;
1200 runs. Raw: `results.csv`.

## Pre-registered verdict: **H-REGIME SUPPORTED** (by the letter)

| cell | W(0), p | W(σ*), p | criterion |
|---|---|---|---|
| func2C n=1000 | 25/25, 6.0e-8 | 14, 0.96 | met |
| func2C n=50 | 25/25, 6.0e-8 | 14, 0.60 | met |
| func3C n=1000 | 25/25, 6.0e-8 | 16, 0.31 | met |
| func3C n=50 | 25/25, 6.0e-8 | 15, 0.13 | met |

Support requires W(0) ≥ 20 & p < 0.05 AND W(σ*) ≤ 17 & p ≥ 0.05 in all
four cells: met in all four. Refute (W(σ*) ≥ 20 & p < 0.05 at n=50, both
benchmarks): not met. Per the fixed ledger consequences, **K10 becomes a
supported claim citing E8**, with E7's INCONCLUSIVE composite honestly
footnoted wherever the claim appears.

Supported claim, stated precisely (wording per review): *within a fixed
pool-argmax search type, the keep-vs-flip generator restriction's effect
on outcomes decays from total (25/25 paired seeds at perfect guidance) to
statistically undetectable at the pre-named surrogate-grade anchor (all
four cells, p ≥ 0.13). The decay is not monotone in σ: small-pool func3C
shows a nominally significant residual (W=18/25, p=.004) at σ=100,
outside the pre-registered criterion — "undetectable" is to be read as
"at the named anchor", not "vanishing at all higher noise".* Combined with E2
(oracle: cap is total and pool-size-scaled) and E3 (real weak surrogates:
no detectable generator effect), this supports the article's diagnostic
use of the oracle audit: the ceiling identifies what machinery *permits*;
proximity to the ceiling determines whether machinery choices *matter*.

## Full dial table and honest caveats (non-anchor cells)

| cell | σ=1 | σ=3 | σ=10 | σ=30 | σ=100 |
|---|---|---|---|---|---|
| func2C n=1000 | 21, 6e-4 | 17, .005 | (14, .96)* | 13, .41 | 15, .81 |
| func2C n=50 | 21, .002 | 15, .56 | (14, .60)* | **18, .030** | 7, .17 |
| func3C n=1000 | 25, 6e-8 | 19, .027 | **20, 8e-4** | (16, .31)* | 12, .98 |
| func3C n=50 | 25, 6e-8 | 22, 2e-5 | **17, .042** | (15, .13)* | **18, .004** |

(* = the pre-named anchor cell used by the criterion; bold = nominally
significant non-anchor cells.)

The decay is real but **not clean monotone**: four non-anchor cells reach
nominal significance, including func3C n=50 at σ=100 (W=18, p=0.004) —
residual generator sensitivity persists under heavy noise in the
small-pool func3C cells. Reading: func3C's larger categorical space (60
combos vs 15) and larger E2 gap mean its "GP-level" anchor sits earlier in
the decay than the σ where sensitivity fully vanishes; and at σ=100 noise
dominates selection *within* the pool, but the pools themselves still
differ (keep pools contain incumbent-combination refinements at all), so a
small persistent edge is mechanistically plausible (untested speculation). These cells were not
part of the pre-registered criterion and do not alter the verdict; they DO
belong in the article as the shape of the phenomenon: **decay to
undetectability at surrogate-grade guidance, with residual small-pool
sensitivity on the benchmark with the larger machinery gap.** No further
K10 experiments (stopping rule).

## Threats to validity

- Multiplicity: 24 cells were tabulated; only the 8 pre-named entered the
  criterion. Nominal p-values in non-anchor cells are descriptive.
- Sequential-design disclosure (per review): E8 retests only the W half of
  E7's composite — the clause that already looked favorable in E7's data —
  after the G half was diagnosed (post-hoc, labeled) as mis-specified. The
  controls for this two-step design are the fresh non-overlapping seeds,
  the criteria being git-committed before the run, and the hard
  no-third-test stopping rule; the reader should weigh the verdict with
  that history in view.
- Anchors were pre-named from E7's keep-referenced pilot; a different
  anchor choice could move individual cells across the 0.05 line — the
  verdict's robustness rests on all four anchor cells being comfortably
  non-significant (min p = 0.13).
- Degradation model remains i.i.d. noise; biased-surrogate degradation
  untested (article limitation, carried from E7).

## Ledger impact (staged, pending independent review)

- K10 → **supported (E8, by pre-registered criterion)** with the precise
  statement above, E7 footnote, and the residual-sensitivity caveat.
- K5's generator-axis clause gains the E8 cross-reference: the E3 null is
  now interpretable as the far-below-ceiling end of a measured decay.
