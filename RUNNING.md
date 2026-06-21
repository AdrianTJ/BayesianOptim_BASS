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
  | TPE baseline (**optional**) | `reticulate` (R) + `optuna` (Python) |

Install the R packages in one go:

```r
install.packages(c(
  "BASS", "GPfit", "lhs", "tidyverse",
  "future", "furrr", "glmnet", "MASS", "testthat"
))
```

**Optional — the TPE baseline.** The `--with_tpe` flag (see §4) adds a
Tree-structured Parzen Estimator baseline via [Optuna](https://optuna.org/),
called from R through `reticulate`. It is entirely optional: without it (or if
`reticulate`/`optuna` are missing) every other method runs exactly as before. To
enable it, install `reticulate` in R and `optuna` in a Python that reticulate can
find:

```bash
Rscript -e 'install.packages("reticulate", repos="https://cloud.r-project.org")'
python3 -m venv ~/.venvs/optuna && ~/.venvs/optuna/bin/pip install optuna
```

Then point reticulate at that Python. The most reliable way (it is inherited by
the parallel workers) is to set it in `~/.Renviron`:

```
RETICULATE_PYTHON=/Users/<you>/.venvs/optuna/bin/python
```

Verify with `Rscript -e 'cat(reticulate::py_module_available("optuna"))'` → `TRUE`.

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
    tpe.R                  #   optional TPE baseline (Optuna, via reticulate)
    config.R               #   default_config() + the --key=value parser
    experiment.R           #   parallel multi-seed harness + summaries + plot
    objectives/            #   branin, rastrigin, synthetic, categorical, + loader
  run_benchmark.R          # entry point for the synthetic benchmarks
  tests/                   # testthat unit tests
  2_tpe_sensitivity/
    run_tpe_sensitivity.R  # entry point for the TPE gamma-sensitivity ablation
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
- **`test-categorical.R`** — the categorical/mixed benchmarks (`func2C`, `func3C`,
  `cat_ackley`), the unit-cube ↔ level decoding, the BASS factor frame, and a loop
  run on a categorical objective.
- **`test-tpe.R`** — the TPE baseline against `run_bo()`'s contract on a continuous
  and a categorical objective (self-skips unless `reticulate` + `optuna` are present).
- **`test-tpe-sensitivity.R`** — `run_tpe()`'s `sampler_opts` argument and
  `run_tpe_sweep_experiment()` (same self-skip behaviour).

Notes:
- The BASS/GP and TPE end-to-end tests self-skip if their dependencies aren't
  installed, so the rest of the suite still runs.
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
| `--objective` | `branin` | `branin`, `rastrigin`, `synthetic`, `func2C`, `func3C`, `cat_ackley` |
| `--d` | `2` | input dimension (Branin must be 2; `func2C`/`func3C` are fixed) |
| `--budget` | `80` | BO iterations after the initial design |
| `--n_cand` | `1000` | candidate points scored per iteration |
| `--reps` | `10` | independent repetitions (seeds) |
| `--seed_start` | `1001` | first seed (`reps` use `seed_start + 0..reps-1`) |
| `--out_dir` | `results` | output folder |
| `--acquisition` | `ei` | BASS acquisition: `ei` or `thompson` (see §7) |
| `--with_tpe` | `false` | add the TPE (Optuna) baseline; needs `reticulate`+`optuna` (§1) |

The categorical/mixed objectives (`func2C`, `func3C`, `cat_ackley`) and the
`--with_tpe` baseline are most informative together — TPE handles categoricals
natively, so it is the strongest comparison there:

```bash
Rscript code_files/run_benchmark.R --objective=func2C --with_tpe=true --out_dir=results_func2C
Rscript code_files/run_benchmark.R --objective=cat_ackley --d=6 --with_tpe=true --out_dir=results_catackley
```

---

## 5. Run the TPE sensitivity ablation

BASS-BO and GP-BO are parameter-free (no exploration weight, no candidate-
generator knob to tune); TPE is not. This script tests the thesis's claim
that TPE has essentially one hyperparameter worth tuning, `gamma`, by
sweeping it across `{0.10, 0.25, 0.50, 0.75}` on one continuous benchmark
(Branin) and one purely categorical one (Cat-Ackley), and plotting the
spread next to the fixed BASS-BO/GP-BO/Random curves. It needs the same
`reticulate`+`optuna` setup as `--with_tpe` above (§1), and aborts up front
if that's unavailable.

```bash
# Default protocol: budget=80, reps=25, seed_start=1001 (matches §4)
Rscript code_files/2_tpe_sensitivity/run_tpe_sensitivity.R

# Faster, smaller sweep for a quick check
Rscript code_files/2_tpe_sensitivity/run_tpe_sensitivity.R --budget=40 --reps=10
```

Writes `results_tpe_sensitivity/branin/` and
`results_tpe_sensitivity/cat_ackley/` (CSVs + convergence plot each, via the
same `save_results()` used by §4), plus a printed "TPE final-best range
across gamma" line quantifying the spread. See
`code_files/2_tpe_sensitivity/README.md` for details.

---

## 6. Run the Elastic Net case study

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

## 7. Acquisition: `ei` vs `thompson`

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

## 8. Outputs

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

## 9. Adding a new objective

1. Add the function (and, if it lives on a physical domain, its `*_bounds`) to
   `code_files/R/objectives/`. Benchmarks on a physical domain are wrapped with
   `vectorize_target()`; objectives already on `[0,1]^d` are used directly.
2. Register the name in `load_objective()` (`code_files/R/objectives.R`).

Nothing in the BO loop needs to change — `run_bo()` is objective-agnostic.

---

## 10. Performance & troubleshooting

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
- **TPE is silently missing from results.** `--with_tpe=true` skips the TPE
  baseline (with a warning) when `reticulate`/`optuna` aren't reachable. Confirm
  `Rscript -e 'cat(reticulate::py_module_available("optuna"))'` prints `TRUE`, and
  set `RETICULATE_PYTHON` in `~/.Renviron` (not just the interactive session) so
  the value is inherited — the run computes TPE in the main process, but the check
  must succeed there. TPE is run sequentially (it is cheap), so it does not need
  Optuna inside the parallel workers.
- **Native pipe `|>` errors.** You're on R < 4.1 — upgrade R.
- **Reproducibility.** Seeds are fixed (`seed_start + 0..reps-1`) and parallel
  RNG is handled by `furrr`, so repeated runs with the same flags match.
```
