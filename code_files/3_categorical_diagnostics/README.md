# Categorical diagnostics

Why doesn't BASS-BO separate from Random Search on the categorical/mixed
benchmarks? This folder contains one script that splits that question into
three independently answerable parts, ordered from the outside in:

1. **Oracle ceiling** (runs without BASS). The acquisition is replaced by the
   true objective, so the loop always picks the best point in the candidate
   pool. This upper-bounds what *any* surrogate could achieve with the
   `hybrid_candidates()` generator, and compares it against a reference
   generator whose local rows may **keep** the incumbent's categorical
   combination. Historically the library generator always flipped at least
   one categorical coordinate, so no candidate ever refined the continuous
   coordinates while holding the best-known combination fixed — on Func-2C/3C
   exactly the local exploitation a model-based method needs to beat Random.
   That is fixed (the library keeps combinations on mixed schemas now), so
   this part remains as a regression check: the two arms should perform alike.

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
| Oracle+current clearly worse than oracle+keep-combo on func2C/3C | Candidate-generator regression: local moves must be able to keep the incumbent's combination on mixed schemas |
| Part-2 Spearman ≈ 0 at n ≈ 90 on cat_ackley (d=6, L=11) | Benchmark is uninformative at this budget; re-scale it rather than tuning methods |
| Easy-mode cat_ackley: BASS-BO does not beat Random | Something is broken in the surrogate/acquisition wiring itself |
| High revisit counts in part 3 | Combination-level dedup regression: the loop is re-evaluating decoded combinations |

## Findings from the first full run (2026-07, pre-fix library)

These diagnostics were run once against the pre-fix library and drove the
fixes now in the shared code:

- **Part 1**: the keep-combo generator beat the always-flip one in **15/15
  paired seeds** on func2C and func3C (reaching the func2C optimum −0.206
  within ~10 evaluations, vs a −0.148 plateau); on purely categorical
  Cat-Ackley both arms reached the optimum, clearing the pool. → fixed in
  `local_categorical_moves()` (zero-flip moves allowed on mixed schemas).
- **Part 2**: held-out Spearman at n ≈ 70: func2C ≈ 0.66, func3C ≈ 0.61,
  cat_ackley d=6/L=11 ≈ 0.47 with near-intercept fits — the hard instance
  cannot show a surrogate advantage at thesis budgets. → Cat-Ackley size is
  now a protocol knob (`--cat_L`), benchmarked at easy/medium/hard.
- **Part 3**: easy-mode Cat-Ackley: BASS-BO found the exact optimum **10/10
  seeds** (Random 5/10) — the method wiring works — but **25–29 of 40 picks
  revisited already-evaluated combinations**. → fixed by combination-level
  dedup (`canonicalize()`). func2C: Random won 6/10 (2 ties) — consistent
  with the Part-1 generator finding. cat_ackley d=6: BASS-BO won 9/10.

**Post-fix expectations**: Part 1's two arms within noise of each other;
Part 3 revisit counts ≈ 0 everywhere; func2C paired wins flipped in
BASS-BO's favour; easy-mode still 10/10.
