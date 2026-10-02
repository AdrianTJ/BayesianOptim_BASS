# H7 — Ax re-run with categorical (unordered) choice parameters

Pre-registration, committed before any run of this experiment.

## Why

The claims audit (`../../claims_audit/REPORT.md`, finding A5) found that
every Ax run in H1 (and therefore the Ax rows carried into H2) treated the
audit's categorical parameters as **ordered** integers. The benchmark
spaces encode levels as integers (`[1..L]`, pest control `[0..4]`); the
driver passed them as `ChoiceParameterConfig(parameter_type="int")`
without `is_ordered`, and Ax 1.3.1 defaults integer choice parameters to
`is_ordered=True`. The other five libraries received the same levels as
nominal categories. A 25-seed probe on 5^5 gave 10/25 solves with
`is_ordered=False` against 18/25 committed. This experiment replaces the
probe with the full pre-registered matrix.

## Change under test

`bo_audit/drivers.py::run_ax` passes `is_ordered=False` for every `"cat"`
dimension. Nothing else changes: same benchmarks, seeds, budget, library
version (Ax 1.3.1), one trial per ask, `Client(random_seed=seed)`. The
recorded `non_defaults` string becomes
`"random_seed; one trial per ask; is_ordered=False on categoricals"`.

## Matrix

Ax only × {cat_ackley_d3_L5, cat_ackley_d5_L5, cat_ackley_d6_L11,
pest_control, func2C, func3C} × seeds 3001–3025 × budget 80 = **150 runs**,
run through the H1 cell runner with the fixed driver. Environment frozen
to `env.txt` (`pip freeze`) before the first run. Results go to
`results.jsonl` here; H1's `results.jsonl` is not touched.

## Pre-registered hypotheses (evaluated by the letter)

- **X1 (dedup unchanged):** Ax registers 0 revisits in all 150 runs.
- **X2 (solve record moves):** on cat_ackley_d5_L5, Ax's solve count is
  lower than the committed ordinal 18/25. (The probe predicts this; it is
  stated so a reversal is visible.)
- **X3 (H2 headline pair):** under the H2 ranking metric (median final
  best, solve-count tie-break), compare optuna-tpe's memoized H2 row with
  nominal Ax on cat_ackley_d5_L5, and optuna-tpe's as-shipped H1 row with
  nominal Ax. Report both orders. No pass/fail: this is the input the
  article's Sec. 6 sentence needs.

## Descriptive outputs

Per benchmark: median revisits, median excess over pigeonhole, solve count
(< 1e-6, on the three Cat-Ackley sizes), median final best, next to the
committed ordinal Ax row. Paired per-seed comparison nominal vs ordinal
(seeds are shared): better / worse / equal counts and Wilcoxon p, labeled
descriptive.

## Rules

- Failures and timeouts are logged in `failures.log`, never dropped.
  Cap: 20 min per run, 45 min on pest_control (H1's caps).
- No result here edits H1, H2 or any committed results file. The article
  changes only after ANALYSIS.md and an independent review.
- If X1 fails, the revisit claims for Ax in the article are reopened
  before anything else.
