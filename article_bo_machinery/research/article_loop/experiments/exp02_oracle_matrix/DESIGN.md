# E2 — Oracle-ceiling matrix (design, written before running)

**Date:** 2026-08-21 · **Purpose:** The article's central experiment at
final protocol: A/B the two machinery axes under the oracle (acquisition =
true objective), so every effect is surrogate-free. Also settle K2a's
open question: does the generator ceiling widen at smaller candidate pools?

## Factors

- **Generator:** `keep` (permissive; zero-flip local rows allowed on mixed
  schemas) vs `flip` (restricted; ≥1 categorical flip forced). Validated
  variants from E1's harness.
- **Dedup:** `combination` (mask on canonicalized points — current library)
  vs `encoding` (mask on raw encodings — the historical leak, pre-fix
  `bo_loop.R`). Requires a new `dedup=` option in `run_bo` (default
  `combination`, E1 behavior unchanged).
- **Pool size (K2a axis):** n_cand ∈ {50, 200, 1000} on the mixed
  benchmarks, keep vs flip at combination-dedup.

## Protocol

Benchmarks: func2C, func3C, cat_ackley d3/L5 (125 combos, solvable),
cat_ackley d6/L11 (1.77M combos). Seeds 1001–1025 (25, paired via shared
per-seed inits). Budget 80, deterministic arm-indexed RNG (E1 review fix).

Part A (dedup × generator, n_cand=1000): 4 benchmarks × 4 cells × 25 seeds,
plus a Random reference arm per benchmark/seed. Instrumentation: per-run
**revisit count** — picks whose canonicalized representation duplicates an
already-evaluated point (tol 1e-10), counted before appending.

Part B (pool axis): func2C/func3C × {keep, flip} × n_cand {50, 200}
× 25 seeds, combination dedup (the 1000 column comes from Part A).

Outputs: `results.csv` (per-run: benchmark, seed, generator, dedup, n_cand,
best@10/40/80, revisits), summary tables in ANALYSIS.md.

## Pre-registered hypotheses

| # | Hypothesis | Support criterion |
|---|---|---|
| H1 | Generator ceiling direction replicates at 25 seeds: keep ≤ flip per seed on mixed benchmarks (combination dedup, n=1000) | keep wins (strictly better final) ≥ 20/25 on func2C and func3C |
| H2 | **Ceiling gap grows as the pool shrinks** (K2a magnitude) | mean(flip−keep) final gap at n_cand=50 > at 200 > at 1000 on both mixed benchmarks; and flip's best@10 degrades faster than keep's |
| H3 | **Dedup leak is invisible in the convergence metric** under the oracle | on every benchmark, |mean final(encoding) − mean final(combination)| < 2× paired SE for both generators |
| H4 | **Dedup leak devours budget silently** | on cat_ackley d3/L5 (oracle, encoding dedup): median revisits ≥ 40/80 picks; with combination dedup: median = 0 |
| H5 | Pure-cat ceilings unaffected by generator | d3/L5 and d6/L11: both generators reach optimum (<0.1) in ≥ 23/25 seeds (combination dedup) |

Falsified hypotheses are reported as such and re-center the ledger — H2 in
particular: if the gap does NOT widen at small pools, K2a's "modest
magnitude" verdict from E1 becomes the final story and the article's framing
must say so.

## Threats

- Oracle-level revisits (H4) overstate surrogate-level waste (the oracle
  re-picks the optimum it already found; a surrogate wastes budget *before*
  finding it) — E5 measures the surrogate-level version; the article must
  present H4 as the mechanism demonstration, not the practical cost figure.
- Part B holds dedup at combination; any pool×dedup interaction is out of
  scope here (noted, not silently dropped).
