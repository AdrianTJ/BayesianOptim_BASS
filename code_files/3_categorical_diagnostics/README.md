# Categorical diagnostics

Why doesn't BASS-BO separate from Random Search on the categorical/mixed
benchmarks? This folder contains one script that splits that question into
three independently answerable parts, ordered from the outside in:

1. **Oracle ceiling** (runs without BASS). The acquisition is replaced by the
   true objective, so the loop always picks the best point in the candidate
   pool. This upper-bounds what *any* surrogate could achieve with the current
   `hybrid_candidates()` generator, and compares it against a patched
   generator whose local rows may **keep** the incumbent's categorical
   combination. The current generator always flips at least one categorical
   coordinate (`local_categorical_moves()` draws `k` from `1..min(3, n_cat)`),
   so no candidate ever refines the continuous coordinates while holding the
   best-known combination fixed — on Func-2C/3C that is exactly the local
   exploitation a model-based method needs to beat Random.

2. **BASS fit quality at BO sample sizes.** Held-out Spearman correlation of
   the BASS posterior mean vs the truth, fit on n0 / n0+20 / n0+budget random
   points. If this is ≈ 0 (plausible for Cat-Ackley with d=6, L=11: 1.77M
   combinations, 66 free level effects, ~90 observations), the benchmark
   cannot show a surrogate advantage at thesis budgets *no matter what the
   acquisition does*, and the honest fix is to re-scale the benchmark (fewer
   levels/dimensions or more budget), not to tune the method.

3. **Instrumented BASS-BO vs Random**, paired per seed (shared initial
   design): per-pick provenance (global LHS half vs local Hamming half),
   revisited-combination counts (wasted evaluations on a deterministic
   objective), paired win rates, and a Wilcoxon signed-rank test. Includes an
   "easy mode" Cat-Ackley (d=3, L=5: 125 combinations) as a pass/fail
   regression: if BASS-BO does not clearly beat Random there, the problem is
   in the method wiring, not benchmark hardness.

## Running

```bash
# Full diagnostic (needs BASS for parts 2-3; part 1 runs regardless)
Rscript code_files/3_categorical_diagnostics/run_diagnostics.R

# Faster pass
Rscript code_files/3_categorical_diagnostics/run_diagnostics.R --reps=5 --budget=40
```

Prints a summary and writes `oracle_ceiling.csv`, `bass_fit_quality.csv`,
`instrumented_runs.csv` under `results/diagnostics/`.

## How to read the outcome

| Observation | Conclusion |
|---|---|
| Oracle+current ≈ Random on func2C/3C, oracle+keep-combo clearly better | Candidate generator is the bottleneck: allow zero-flip local moves on mixed problems |
| Part-2 Spearman ≈ 0 at n ≈ 90 on cat_ackley (d=6, L=11) | Benchmark is uninformative at this budget; re-scale it rather than tuning methods |
| Easy-mode cat_ackley: BASS-BO does not beat Random | Something is broken in the surrogate/acquisition wiring itself |
| High revisit counts in part 3 | EI keeps re-picking already-evaluated combinations; deduplicate at the decoded-combination level |
