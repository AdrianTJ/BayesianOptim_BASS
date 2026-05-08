# Bayesian Optimization with Bayesian Adaptive Spline Surfaces (BASS-BO)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-Project](https://img.shields.io/badge/R-Project-blue.svg)](https://www.r-project.org/)
[![Python](https://img.shields.io/badge/Python-3.x-blue.svg)](https://www.python.org/)
[![Version](https://img.shields.io/badge/Release-v0.8.1-orange.svg)](https://github.com/AdrianTJ/BayesianOptim_BASS/releases)

This repository contains the research and implementation for a Master's thesis in Data Science focused on **Bayesian Active Supervised Learning for Optimal Experimental Design (BASS-BO)**. The project investigates the efficacy of **Bayesian Adaptive Spline Surfaces (BASS)** as a surrogate model for Bayesian Optimization (BO), providing a flexible, non-parametric alternative to traditional Gaussian Processes (GP).

## Abstract

Bayesian Optimization is a powerful framework for optimizing expensive-to-evaluate black-box functions. While Gaussian Processes are the industry standard surrogate model, they often struggle with high-dimensional spaces and non-stationary surfaces. This project explores the use of BASS—a Bayesian extension of Multivariate Adaptive Regression Splines (MARS)—to model the objective function's response surface. 

BASS-BO leverages piecewise linear basis functions (hinge functions) to capture complex interactions and non-linearities without the cubic scaling issues typically associated with GPs. Our experimental results compare BASS-BO against GP-BO, Random Search, and Grid Search across various synthetic benchmark functions and real-world regression hyperparameter tuning tasks.

## Key Features

- **BASS Surrogate Modeling**: Implementation of BASS as the underlying engine for Sequential Model-Based Optimization (SMBO).
- **Comparative Analysis**: Rigorous benchmarking against standard GP-BO and baseline search strategies.
- **Configurable Exploration/Exploitation**: Advanced acquisition function controls, including exploration schedules, local refinement sampling, and uncertainty inflation factors.
- **Multi-Dimensional Support**: Evaluated on $2d$ (Branin-Hoo), $4d$ (Rastrigin), and higher-dimensional regression test cases.
- **Reproducible Framework**: Comprehensive scripts for parallel target evaluation and result aggregation.

## Mathematical Intuition

The core of BASS lies in its use of **hinge functions** of the form $(x - t)_+$ and $(t - x)_+$. By combining these into an additive model with interaction terms, BASS can approximate any continuous function. The Bayesian approach (BMARS) allows for robust uncertainty estimation via MCMC sampling, which is critical for the acquisition function (e.g., Lower Confidence Bound) to guide the optimization process effectively.

$$f(X) = \beta_0 + \sum_{m=1}^M \beta_m h_m(X)$$

where $h_m(X)$ represents a basis function or a product of hinge functions.

## Repository Structure

```text
├── class_presentation/      # Academic dissemination materials
│   ├── Presentacion/        # Beamer slides (LaTeX) for thesis defense
│   └── ReporteFinal/        # Final project report and technical summaries
├── code_files/              # Core implementation and experiments
│   ├── 1_base_loop/         # Core BO loop logic and initial comparisons
│   ├── 2_generating_iterations/ # Execution scripts for BASS-BO runs
│   ├── 3_test_functions/    # Benchmark targets (Branin, Rastrigin, etc.)
│   ├── 4_regression_test_case/ # Real-world application (Elastic Net tuning)
│   └── figure_generations/  # Python/R scripts for thesis visualizations
├── written_files/           # Thesis documentation
│   └── tesis_escrito/       # Main LaTeX source for the thesis document
└── README.md                # Project overview and documentation
```

## Getting Started

### Prerequisites

The project primarily uses **R** for the optimization loops and **Python** for specific visualization components.

- **R Packages**: `BASS`, `GPfit`, `lhs`, `tidyverse` (ggplot2, dplyr), `future`, `furrr`, `gt`
- **Python Libraries**: `numpy`, `scipy`, `matplotlib`, `scikit-learn`

### Running Experiments

To reproduce the benchmark results for the Branin function:

1. Navigate to `code_files/3_test_functions/`.
2. Execute the parallel runner:
   ```bash
   Rscript run_parallel_target.R --target branin --iterations 80 --seeds 25
   ```

## Results

Experiments indicate that BASS-BO demonstrates competitive sample efficiency, particularly in landscapes where the underlying function exhibits piecewise linear behavior or sharp transitions that GPs may over-smooth. Detailed convergence plots and statistical summaries can be found in `code_files/3_test_functions/results_branin_b80/`.

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
