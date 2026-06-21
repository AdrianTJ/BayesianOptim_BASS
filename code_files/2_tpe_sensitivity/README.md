# TPE hyperparameter sensitivity

BASS-BO and GP-BO are parameter-free: there is no exploration weight, no
kappa, no candidate-generator setting to tune (see `R/config.R` and
Experiment.tex, "Acquisition and Candidate Generation"). TPE is not --
Optuna's `TPESampler` exposes several sampler hyperparameters, and the
thesis (`Surrogate_Models.tex`) singles out `gamma`, the quantile of trials
treated as "good," as the one that matters. This experiment makes that claim
testable: it sweeps `gamma` over `{0.10, 0.25, 0.50, 0.75}` and plots the
resulting TPE curves next to the single, un-sweepable BASS-BO/GP-BO/Random
curves, on one continuous benchmark (Branin, 2D) and one purely categorical
one (Cat-Ackley, 6D) -- the two regimes TPE is compared against BASS-BO on
elsewhere in the suite.

TPE has no MCMC fit per iteration, so the whole sweep (4 gamma values x 2
objectives x `reps` seeds) is cheap relative to the main benchmark suite.

## Running

```bash
# Default protocol: budget=80, reps=25, seed_start=1001 (matches the main
# benchmark suite for a direct comparison)
Rscript code_files/2_tpe_sensitivity/run_tpe_sensitivity.R

# Faster, smaller sweep for a quick check
Rscript code_files/2_tpe_sensitivity/run_tpe_sensitivity.R --budget=40 --reps=10
```

Needs `reticulate` + an importable `optuna` Python module, same as the
`--with_tpe` baseline (see `RUNNING.md` Sec. 1); the script aborts up front
if TPE is unavailable, since there is nothing else for it to do.

## Output

Writes `results_tpe_sensitivity/branin/` and
`results_tpe_sensitivity/cat_ackley/`, each with the usual
`all_runs.csv`, `summary_curve.csv`, `final_summary.csv`, and
`convergence_mean_ci.png` (via the shared `save_results()`), plus a
printed "TPE final-best range across gamma" spread -- the number that
quantifies how much gamma moves TPE's outcome, in contrast to the fixed
BASS-BO/GP-BO baselines.

This result feeds the "TPE Hyperparameter Sensitivity" subsection in
`written_files/tesis_escrito/TeX_files/Experiment.tex`.
