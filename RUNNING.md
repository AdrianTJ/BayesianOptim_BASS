# Running the BASS-BO Code

A practical, copy-paste guide to running the test suite and every experiment.
All commands are run from the **repository root** unless noted.

---

## 1. Prerequisites

- **R ≥ 4.1** (the code uses the native `|>` pipe).
- The following CRAN packages:

  | Purpose | Packages |
  |---|---|
  | Core optimisation | `BASS`, `GPfit`, `lhs` |
  | Data wrangling / plotting | `tidyverse` (`dplyr`, `tibble`, `readr`, `ggplot2`, `purrr`) |
  | Parallelism | `future`, `furrr` |
  | Elastic Net case study | `glmnet`, `MASS` |
  | Tests | `testthat` |

Install them all in one go:

```r
install.packages(c(
  "BASS", "GPfit", "lhs", "tidyverse",
  "future", "furrr", "glmnet", "MASS", "testthat"
))
```

Check your setup:

```bash
Rscript -e 'cat(R.version.string, "\n"); for (p in c("BASS","GPfit","lhs","tidyverse","future","furrr","glmnet","MASS","testthat")) cat(sprintf("%-10s %s\n", p, requireNamespace(p, quietly=TRUE)))'
```

---

## 2. What lives where

```
code_files/
  R/                       # the shared library (plain .R files, sourced)
    bo_loop.R              #   run_bo() — the one generic BO loop + make_methods()
    surrogates.R           #   BASS & GP, each as an Expected-Improvement closure
    acquisition.R          #   Expected Improvement (closed-form + Monte Carlo)
    candidates.R           #   candidate generation + duplicate handling
    config.R               #   default_config() + the --key=value parser
    experiment.R           #   parallel multi-seed harness + summaries + plot
    objectives/            #   branin, rastrigin, synthetic, + the loader
  run_benchmark.R          # entry point for the synthetic benchmarks
  tests/                   # testthat unit tests
  4_regression_test_case/
    run_elastic_net.R      # entry point for the Elastic Net case study
    enet_objective.R       #   the CV-RMSE objective (alpha, lambda)
```

The library has **no build step** — the runners `source()` it.

---

## 3. Run the test suite

```bash
Rscript code_files/tests/run_tests.R
```

This runs all `testthat` tests under `code_files/tests/testthat/`:

- **`test-acquisition.R`** — Expected Improvement maths (closed-form & Monte Carlo).
- **`test-candidates.R`** — candidate shapes/ranges and duplicate detection.
- **`test-objectives.R`** — domain scaling, target vectorisation, Branin's known
  minimum, and the objective loader.
- **`test-bo_loop.R`** — the `run_bo()` loop on a convex problem (must improve
  monotonically), Random Search, and a guarded end-to-end check of the **real**
  BASS (EI and Thompson) and GP surrogates.

Notes:
- The BASS/GP end-to-end test self-skips if those packages aren't installed, so
  the rest of the suite still runs.
- A non-zero exit code means a failure (handy for CI).

---

## 4. Run the synthetic benchmarks

One entry point compares **BASS-BO**, **GP-BO** and **Random Search** on a chosen
objective, then writes CSVs and a convergence plot.

```bash
# Branin (2-D), 80 BO iterations, 25 repetitions
Rscript code_files/run_benchmark.R --objective=branin --d=2 --budget=80 --reps=25

# Rastrigin (4-D)
Rscript code_files/run_benchmark.R --objective=rastrigin --d=4 --reps=10

# The hand-built non-smooth surface (any dimension), custom output folder
Rscript code_files/run_benchmark.R --objective=synthetic --d=3 --out_dir=results_syn

# Quick smoke run while iterating
Rscript code_files/run_benchmark.R --objective=branin --budget=15 --reps=3
```

### Command-line flags

Every key in `default_config()` (`code_files/R/config.R`) is settable as
`--key=value`. A typo'd flag errors instead of being silently ignored.

| Flag | Default | Meaning |
|---|---|---|
| `--objective` | `branin` | `branin`, `rastrigin`, or `synthetic` |
| `--d` | `2` | input dimension (Branin must be 2) |
| `--budget` | `80` | BO iterations after the initial design |
| `--n_cand` | `1000` | candidate points scored per iteration |
| `--reps` | `10` | independent repetitions (seeds) |
| `--seed_start` | `1001` | first seed (`reps` use `seed_start + 0..reps-1`) |
| `--out_dir` | `results` | output folder |
| `--acquisition` | `ei` | BASS acquisition: `ei` or `thompson` (see §6) |

---

## 5. Run the Elastic Net case study

Tunes an Elastic Net (`alpha`, `lambda`) on the Boston Housing data by minimising
cross-validated RMSE, then evaluates the chosen model on a held-out test set.
It reuses the **same** optimisers as the synthetic benchmarks.

```bash
Rscript code_files/4_regression_test_case/run_elastic_net.R --reps=50 --budget=100
```

Extra flags (on top of the shared ones above):

| Flag | Default | Meaning |
|---|---|---|
| `--nfolds` | `5` | CV folds used by the objective |
| `--train_frac` | `0.8` | train/test split fraction |
| `--lambda_log10_min` | `-5` | low end of the log10(lambda) grid |
| `--lambda_log10_max` | `1` | high end of the log10(lambda) grid |
| `--cache_digits` | `6` | rounding precision for objective caching |

---

## 6. Acquisition: `ei` vs `thompson`

Both surrogates are scored with **Expected Improvement** — closed-form for the
GP, and Monte Carlo (straight from the posterior draws) for BASS. This is
parameter-free: there is no exploration weight to tune.

- `--acquisition=ei` (default) — averages improvement over BASS's full posterior.
  The principled, most robust choice.
- `--acquisition=thompson` — draws a single posterior surface and heads to its
  minimum. Predicts one draw instead of ~200, so it is noticeably faster, at the
  cost of a bit more per-step variance.

The GP baseline always uses closed-form EI, so the **only** difference between
BASS-BO and GP-BO is the surrogate model.

---

## 7. Outputs

Each synthetic run writes to `--out_dir` (default `results/`):

| File | Contents |
|---|---|
| `all_runs.csv` | best-so-far for every method, seed, and iteration (long format) |
| `summary_curve.csv` | per-iteration mean best ± 95% CI, by method |
| `final_summary.csv` | each method's final best (mean ± sd), ranked |
| `convergence_mean_ci.png` | mean convergence curves with CI ribbons |

The Elastic Net run additionally writes `final_summary_cv.csv`,
`best_params_and_test_rmse_by_seed.csv`, and `test_rmse_summary.csv`
(the held-out test performance of each method's chosen model).

> Result folders are **git-ignored** and regenerated on demand — they are not
> committed.

---

## 8. Adding a new objective

1. Add the function (and, if it lives on a physical domain, its `*_bounds`) to
   `code_files/R/objectives/`. Benchmarks on a physical domain are wrapped with
   `vectorize_target()`; objectives already on `[0,1]^d` are used directly.
2. Register the name in `load_objective()` (`code_files/R/objectives.R`).

Nothing in the BO loop needs to change — `run_bo()` is objective-agnostic.

---

## 9. Performance & troubleshooting

- **Parallelism.** Runs fan out across seeds using `future`/`furrr`, using
  `cores - 1` workers. Lower `--reps` (or the worker count via the `future` plan
  in the runner) if memory is tight.
- **Speed knobs.** BASS MCMC settings are internal constants at the top of
  `code_files/R/surrogates.R` (`BASS_NMCMC`, `BASS_NBURN`, `BASS_THIN`). Raise
  them for smoother posteriors in final thesis runs; lower them for quick checks.
  `--acquisition=thompson` and a smaller `--n_cand` also speed things up.
- **`could not find function` / `predict` errors in workers.** Make sure the
  packages in §1 are installed for the same R that runs the scripts; the runners
  load the surrogate namespaces on each worker.
- **Native pipe `|>` errors.** You're on R < 4.1 — upgrade R.
- **Reproducibility.** Seeds are fixed (`seed_start + 0..reps-1`) and parallel
  RNG is handled by `furrr`, so repeated runs with the same flags match.
```
