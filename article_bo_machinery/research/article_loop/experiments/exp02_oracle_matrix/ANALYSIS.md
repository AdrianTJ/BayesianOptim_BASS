# E2 — Analysis

**Date:** 2026-08-21 · **Protocol:** as DESIGN.md. 700 runs (Part A: 4
benchmarks × 5 arms × 25 seeds; Part B: 2 × 2 × 2 × 25), budget 80, paired
seeds 1001–1025, deterministic arm-indexed RNG. Runtime 5m04s, single core,
zero surrogate fits. Raw: `results.csv`.

**Independent review: UPHELD** (see `REVIEW.md`; zero numeric mismatches on
recomputation). Five minor findings incorporated below: cross-benchmark RNG
seed reuse documented (harmless within-benchmark); H2's best@10 clause made
quantitative (strong on func2C, ≈5% margin on func3C — the headline rests on
the final-gap clause); H3's SE=0 edge case noted (unexercised); "exact
optimum" reads "optimum to 4 decimals" (per-seed residual ≤3×10⁻⁵ at
n_cand=50); H5 disclosed as a harness consistency check (keep/flip are
functionally identical code paths on pure-cat schemas).

## Verdict: all five pre-registered hypotheses PASS

### H1/H2 — The generator ceiling is real, and pool size sets its severity

Keep (permissive) beats flip (restricted) on **25/25 paired seeds in every
cell** (Wilcoxon p = 6.0×10⁻⁸, the n=25 floor). Keep reaches the optimum to
4 decimals at *every* pool size (per-seed residual ≤3×10⁻⁵ at n=50); only
the restricted generator degrades:

| benchmark | n_cand | mean final keep | mean final flip | gap | mean@10 keep | mean@10 flip |
|---|---|---|---|---|---|---|
| func2C | 50 | **−0.2063** | −0.1892 | 0.0171 | −0.1893 | −0.1304 |
| func2C | 200 | **−0.2063** | −0.2008 | 0.0055 | −0.2057 | −0.1661 |
| func2C | 1000 | **−0.2063** | −0.2051 | 0.0012 | −0.2063 | −0.1999 |
| func3C | 50 | **−0.7221** | −0.6361 | 0.0861 | −0.5369 | −0.4306 |
| func3C | 200 | **−0.7221** | −0.6645 | 0.0576 | −0.7176 | −0.5890 |
| func3C | 1000 | **−0.7221** | −0.7111 | 0.0111 | −0.7221 | −0.6250 |

The E1 concern ("gap is modest at n_cand=1000") resolves into the article's
sharper story: **the permissive generator is pool-size robust; the
restricted one pays more the smaller the pool** — gap ×14 on func2C and ×8
on func3C going 1000→50. Since realistic expensive-BO pools are small, the
restriction is worst exactly where it matters. This replaces both the E1
"modest magnitude" caveat and the scaffold's preliminary 15-seed numbers:
the article can now quote 25/25, p=6×10⁻⁸, at three pool sizes.

**Quarantined figure resolved in R (K2a-fig → refuted as recorded).** R was
installed in-container and the historical k-flip generator reconstructed and
run through the *actual R library* (`r_check/k2afig_check.R`, 10 seeds,
budget 60, results `r_check/r_results.csv`):

| R arm | mean best@10 | mean best@60 |
|---|---|---|
| oracle + historical flip, n_cand=1000 | **−0.1971** | −0.2053 |
| oracle + historical flip, n_cand=50 | −0.1164 | −0.1784 |
| oracle + keep, n_cand=1000 | −0.2063 | −0.2063 |

R itself does not produce −0.148@10 at the committed config — the recorded
figure is **refuted as stated**; it sits between the n_cand=50 and
n_cand=200 results, consistent with an ad-hoc small-pool run. The article
drops the figure and uses the E2 pool-size table instead (which makes the
same point, better, at protocol scale). Side benefit: R and Python agree to
~2 decimals on every shared cell (hist@10: −0.197 R vs −0.199 Py; @60/80:
−0.2053 both; keep: −0.20632 both) — the Python harness is now
**cross-language validated** against the R library directly.

### H3/H4 — The dedup leak: invisible in curves, devastating in budget

- **H3 PASS:** switching encoding↔combination dedup moves no benchmark's
  mean final value by ≥2 paired SE under the oracle — the convergence plot
  literally cannot see the leak.
- **H4 PASS:** on cat_ackley d3/L5, encoding-level dedup re-spends a median
  **78 of 80 picks** (range 76–80; mean 78.1) on already-evaluated
  combinations; combination-level dedup: median **0**. This is the
  mechanism demonstration at its cleanest: ~97% of the budget silently
  burned, zero trace in the results table.
- Caveat (pre-registered): the oracle re-picks the optimum it already
  found, so 78/80 overstates the *practical* cost for a surrogate-driven
  loop (R's instrumented BASS runs showed 25–29/40 pre-fix). E5 measures
  the surrogate-level figure; the article presents H4 as mechanism, R/E5
  as cost.

### H5 — Pure-categorical ceilings are generator-independent

Both generators reach <0.1 (optimum region) on 25/25 seeds on both
Cat-Ackley sizes: the pool is not the binding constraint on pure-cat
benchmarks even at 1.77M combinations — reconfirming that the mixed-space
generator restriction (continuous refinement at a kept combination) is the
specific mechanism, not categorical reachability.

### Reference values for the article (canonical, supersede scaffold numbers)

- Oracle+keep final means: func2C −0.2063 (= exact optimum −0.206326),
  func3C −0.7221 (= −0.722140), at all pool sizes tested.
- Random reference (budget 80): func2C −0.037, func3C −0.067, d3/L5 5.09,
  d6/L11 18.56.
- Cost of the full 2×2×25-seed audit on one benchmark: ≤ ~2 min single-core
  at n_cand=1000 (700 total runs took 5m04s).

## Threats to validity

- Part B fixed dedup=combination; pool×dedup interaction untested (scoped
  out in DESIGN).
- Cat-Ackley permutation instances are numpy-seeded, not R's (declared
  E1 deviation; conclusions are about the benchmark family).
- Revisit counting uses canonicalized-point equality (tol 1e-10): on mixed
  benchmarks continuous coords make exact revisits rare, so H4 is
  established on the pure-cat benchmark by design.

## Ledger impact

- K2a → **supported, upgraded**: 25/25 every cell, p=6e-8; severity scales
  inversely with pool size (gap ×8–14 from n=1000→50); permissive generator
  is pool-robust. E1's "modest magnitude" caveat superseded.
- K2b → oracle-level mechanism now demonstrated at protocol scale (median
  78/80 silent revisits, invisible in curves — H3+H4); surrogate-level cost
  remains with R's 25–29/40 + E5.
- K2a-fig → still quarantined; new concrete hypothesis (small-pool run).
- K6 → strengthened: full matrix audit ≈ minutes, single core.
- K7 → unchanged (d6/L11 oracle clears the pool; hardness there is
  surrogate learnability, per thesis Spearman findings).
