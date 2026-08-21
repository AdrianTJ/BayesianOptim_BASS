# E1 — Harness validation (design, written before running)

**Date:** 2026-08-21 · **Hypothesis:** A Python reimplementation of the R
pipeline's machinery (schema encoding, hybrid candidate generator in both
historical variants, combination-level dedup, oracle loop) reproduces the R
diagnostics' oracle-ceiling findings statistically. Until it does, no new
Python-derived claim enters the ledger.

## What is reimplemented (reference: `code_files/R/`)

- `decode_levels(u,L) = min(floor(u·L)+1, L)`; canonicalization snaps
  categorical coords to bin centres `(level−0.5)/L` (candidates.R).
- `hybrid_candidates`: global half = random LHS; local half = Gaussian cloud
  around the incumbent with data-derived scale (nearest-neighbour distance
  clipped to [0.01, 0.5]; 0.1 if <2 points), categorical coords replaced by
  Hamming moves: each flips w.p. 1/n_cat to a uniform *other* level.
  - **Variant `keep` (permissive / current library):** zero-flip rows allowed
    on mixed schemas (pure continuous refinement at the incumbent combo);
    ≥1 flip forced only on purely categorical schemas.
  - **Variant `flip` (restricted / historical):** ≥1 flip always forced.
- Loop (bo_loop.R): score pool, mask candidates duplicating canonicalized
  evaluated points (tol 1e-10), argmax with uniform random tie-break;
  Random = up to 100 uniform draws for a non-duplicate. Oracle acquisition
  = −f(candidates).
- Objectives: Func-2C, Func-3C (CoCaBO forms, exact constants), Cat-Ackley
  (permuted-categorical Ackley, grid ±32.768, odd L, optimum 0).

## Acknowledged deviations (cannot be bitwise-identical)

1. RNG streams differ (R Mersenne sample/runif vs numpy Generator) — so
   Cat-Ackley's per-input permutations differ from R's. The benchmark family
   is the same; the specific instance is not. All comparisons are therefore
   **statistical**, not per-seed-identical to R.
2. Initial designs: scipy LatinHypercube vs R `lhs::maximinLHS`. Same class
   of space-filling design; paired *within* Python (both arms share each
   seed's design), as in R.

## Protocol

Benchmarks: func2C (d=4), func3C (d=5), cat_ackley (d=6, L=11, pure-cat).
Arms per seed (shared initial design): oracle+keep, oracle+flip, random.
Seeds: 15 (1001–1015). Budget 80, n_cand 1000, n0 = max(2d+1, 8).
Outputs: per-seed best-so-far curves → `results.csv`; summary → ANALYSIS.md.

## Pre-registered pass criteria (from the R diagnostics README / scaffold)

| # | Check | Threshold |
|---|---|---|
| V1 | func2C oracle+keep final best | mean within 1e-3 of −0.2063; reaches −0.20 by ≤ 15 evals (R: ~10) |
| V2 | func2C oracle+flip early plateau | mean best at budget 10 in [−0.19, −0.11] (R: ≈ −0.148) |
| V3 | func2C paired wins keep vs flip (budget 80) | keep strictly better in ≥ 12/15 seeds (R: 15/15) |
| V4 | func3C keep vs flip | keep mean within 2e-3 of −0.7216; flip mean in [−0.72, −0.67] (R: ≈ −0.697); keep wins ≥ 12/15 |
| V5 | cat_ackley d6 L11 (pure-cat) | both oracle arms reach < 0.1 by budget 80 in ≥ 13/15 seeds (R: both reached optimum) |
| V6 | Random arm sanity | random final best worse (higher) than both oracle arms on every benchmark's mean |

**Falsification:** any V-check failing → the harness (or our reading of the
R code) is wrong; fix and re-run before anything downstream. Partial passes
are reported check-by-check, never averaged away.
