# Bayesian Optimization with Bayesian Adaptive Spline Surfaces (BASS-BO)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-Project](https://img.shields.io/badge/R-Project-blue.svg)](https://www.r-project.org/)
[![Python](https://img.shields.io/badge/Python-3.x-blue.svg)](https://www.python.org/)
[![Version](https://img.shields.io/badge/Release-v0.8.1-orange.svg)](https://github.com/AdrianTJ/BayesianOptim_BASS/releases)

This repository contains the research and implementation for a Master's thesis in Data Science focused on **Bayesian Adaptive Spline Surfaces for Bayesian Optimization (BASS-BO)**. The project investigates the efficacy of **Bayesian Adaptive Spline Surfaces (BASS)** as a surrogate model for Bayesian Optimization (BO), providing a flexible, non-parametric alternative to traditional Gaussian Processes (GP).

## Abstract

Bayesian Optimization is a powerful framework for optimizing expensive-to-evaluate black-box functions. While Gaussian Processes are the industry standard surrogate model, they often struggle with high-dimensional spaces and non-stationary surfaces. This project explores the use of BASS, a Bayesian extension of Multivariate Adaptive Regression Splines (MARS), to model the objective function's response surface. 

BASS-BO leverages piecewise linear basis functions (hinge functions) to capture complex interactions and non-linearities without the cubic scaling issues typically associated with GPs. A further advantage, and the one this project emphasizes, is that BASS handles **categorical inputs natively** (as factors), where a standard GP kernel cannot. Our experimental results compare BASS-BO against GP-BO, Random Search, and an optional Tree-structured Parzen Estimator (TPE) baseline across synthetic benchmark functions—continuous, categorical, and mixed—and real-world regression hyperparameter tuning tasks.

## Key Features

- **BASS Surrogate Modeling**: Implementation of BASS as the underlying engine for Sequential Model-Based Optimization (SMBO).
- **Parameter-free acquisition**: Both surrogates use **Expected Improvement**: closed-form for the GP, and **Monte Carlo EI computed directly from BASS's posterior draws** for BASS (with an optional single-draw Thompson-sampling variant). There are no exploration weights, uncertainty-inflation factors, or annealing schedules to tune, so the surrogate is the only thing that differs between methods.
- **Comparative Analysis**: Rigorous benchmarking against standard GP-BO and baseline search strategies.
- **Multi-Dimensional Support**: Evaluated on $2d$ (Branin-Hoo), $4d$ (Rastrigin), and higher-dimensional regression test cases.
- **Reproducible Framework**: Comprehensive scripts for parallel target evaluation and result aggregation.

## Mathematical Intuition

The core of BASS lies in its use of **hinge functions** of the form $(x - t)_+$ and $(t - x)_+$. By combining these into an additive model with interaction terms, BASS can approximate any continuous function. The Bayesian approach (BMARS) yields a full posterior over response surfaces via MCMC sampling. We exploit this directly: rather than collapsing the posterior to a mean and standard deviation, we draw posterior samples and compute **Expected Improvement by Monte Carlo**, so the acquisition uses BASS's true (non-Gaussian) predictive distribution to guide the search.

$$f(X) = \beta_0 + \sum_{m=1}^M \beta_m h_m(X)$$

where $h_m(X)$ represents a basis function or a product of hinge functions.

## Repository Structure

```text
├── class_presentation/      # Academic dissemination materials
│   ├── Presentacion/        # Beamer slides (LaTeX) for thesis defense
│   └── ReporteFinal/        # Final project report and technical summaries
├── code_files/              # Core implementation and experiments
│   ├── R/                   # Shared BO library (sourced, no build step)
│   │   ├── bo_loop.R        #   one generic run_bo() loop + method definitions
│   │   ├── surrogates.R     #   BASS and GP surrogates as Expected-Improvement closures
│   │   ├── candidates.R     #   candidate generators + duplicate detection
│   │   ├── acquisition.R    #   Expected Improvement (closed-form + Monte Carlo)
│   │   ├── tpe.R            #   optional TPE baseline (Optuna, via reticulate)
│   │   ├── config.R         #   default_config() + --key=value CLI parser
│   │   ├── experiment.R     #   parallel multi-seed harness + summaries + plot
│   │   └── objectives/      #   Branin, Rastrigin, synthetic, and categorical/mixed
│   ├── run_benchmark.R      # Single entry point for the synthetic benchmarks
│   ├── tests/               # testthat unit-test suite for the library
│   ├── 1_base_loop/         # Exploratory R Markdown notebooks (pedagogical)
│   ├── 2_tpe_sensitivity/   # Ablation: TPE's gamma sensitivity vs. parameter-free BASS-BO/GP-BO
│   │   └── run_tpe_sensitivity.R #   driver (reuses the shared library)
│   ├── 4_regression_test_case/ # Real-world case study: Elastic Net tuning
│   │   ├── run_elastic_net.R   #   driver (reuses the shared library)
│   │   └── enet_objective.R    #   CV-RMSE objective over (alpha, lambda)
│   └── figure_generations/  # Python/R scripts for thesis visualizations
├── written_files/           # Thesis documentation
│   └── tesis_escrito/       # Main LaTeX source for the thesis document
└── README.md                # Project overview and documentation
```

## Getting Started

### Prerequisites

The project primarily uses **R** for the optimization loops and **Python** for specific visualization components.

- **R Packages**: `BASS`, `GPfit`, `lhs`, `tidyverse` (ggplot2, dplyr, readr, tibble), `future`, `furrr`, `testthat`; and optionally `reticulate` for the TPE baseline.
- **Python Libraries**: `numpy`, `scipy`, `matplotlib`, `scikit-learn`; and optionally `optuna` (used by the `--with_tpe` baseline through `reticulate`).

### Running Experiments

All synthetic benchmarks run through a single entry point, `run_benchmark.R`,
which compares BASS-BO, GP-BO and Random Search on one objective and writes the
CSVs plus a convergence plot. Any setting in `default_config()` (see
`code_files/R/config.R`) can be overridden with a `--key=value` flag.

```bash
# Branin (2-D), 80 iterations, 25 repetitions
Rscript code_files/run_benchmark.R --objective=branin --d=2 --budget=80 --reps=25

# Rastrigin (4-D)
Rscript code_files/run_benchmark.R --objective=rastrigin --d=4 --reps=10

# The hand-built non-smooth surface (any dimension)
Rscript code_files/run_benchmark.R --objective=synthetic --d=3

# Use the fast single-draw Thompson-sampling acquisition for BASS instead of EI
Rscript code_files/run_benchmark.R --objective=branin --acquisition=thompson

# Categorical / mixed benchmarks, with the categorical-capable TPE baseline added
# (needs reticulate + optuna; see RUNNING.md §1)
Rscript code_files/run_benchmark.R --objective=func2C --with_tpe=true
Rscript code_files/run_benchmark.R --objective=cat_ackley --d=6 --with_tpe=true
```

A separate ablation, `code_files/2_tpe_sensitivity/run_tpe_sensitivity.R`,
sweeps TPE's `gamma` hyperparameter and compares the spread against the
parameter-free BASS-BO/GP-BO baselines (see that directory's `README.md`):

```bash
Rscript code_files/2_tpe_sensitivity/run_tpe_sensitivity.R
```

The real-world Elastic Net case study uses the **same** optimisers via a small
driver of its own:

```bash
Rscript code_files/4_regression_test_case/run_elastic_net.R --reps=50 --budget=100
```

> Results are generated on demand and are not committed to the repository. See
> [`RUNNING.md`](RUNNING.md) for a full, step-by-step guide (dependencies, every
> command, outputs, and tips).

### Running the Tests

The library ships with a `testthat` unit-test suite:

```bash
Rscript code_files/tests/run_tests.R
```

### Adding a New Objective

1. Drop a function (and, for a benchmark on a physical domain, its bounds) in
   `code_files/R/objectives/`.
2. Register its name in `load_objective()` (`code_files/R/objectives.R`).

No changes to the BO loop are needed; it is agnostic to the objective.

## Results

Experiments indicate that BASS-BO demonstrates competitive sample efficiency, particularly in landscapes where the underlying function exhibits piecewise linear behavior or sharp transitions that GPs may over-smooth. Convergence plots and statistical summaries are written to the chosen `--out_dir` when you run the benchmarks (they are not committed); see [`RUNNING.md`](RUNNING.md).

## Citation

If you use this work in your research, please cite:

```bibtex
@mastersthesis{AdrianTJ2024BASSBO,
  author       = {Adrian TJ},
  title        = {Bayesian Active Supervised Learning for Optimal Experimental Design (BASS-BO)},
  school       = {ITAM},
  year         = {2024},
  type         = {Master's Thesis}
}
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
