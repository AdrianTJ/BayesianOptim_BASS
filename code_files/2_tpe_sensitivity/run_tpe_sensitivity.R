#!/usr/bin/env Rscript

# =============================================================================
# run_tpe_sensitivity.R  --  Is TPE's "no tuning needed" claim actually true?
# =============================================================================
# BASS-BO and GP-BO are parameter-free by construction (see R/config.R and
# the "Acquisition and Candidate Generation" section of Experiment.tex): there
# is no exploration weight, no kappa, no candidate-generator knob to set.
# TPE is not -- Optuna's TPESampler exposes
# several sampler-level hyperparameters, and the thesis (Surrogate_Models.tex)
# singles out one in particular: gamma, the quantile of trials treated as
# "good" versus "bad". This script tests that claim empirically: it sweeps
# gamma across a small grid and plots the resulting TPE curves alongside the
# (un-sweepable, single-curve) BASS-BO and GP-BO references, on one continuous
# benchmark (Branin) and one purely categorical one (Cat-Ackley) -- the two
# regimes TPE is pitched against BASS-BO on.
#
# TPE is cheap (no MCMC fit per iteration), so the whole sweep -- four gamma
# values x two objectives x reps seeds -- is inexpensive relative to the main
# benchmark suite.
#
# Example:
#   Rscript code_files/2_tpe_sensitivity/run_tpe_sensitivity.R
#   Rscript code_files/2_tpe_sensitivity/run_tpe_sensitivity.R --budget=40 --reps=10
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lhs)
  library(BASS)
  library(GPfit)
  library(future)
  library(furrr)
})

# --- Load the shared BO library, relative to this script's location ----------
this_file  <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_dir <- if (length(this_file)) dirname(normalizePath(this_file)) else getwd()
lib_dir    <- normalizePath(file.path(script_dir, "..", "R"))

source(file.path(lib_dir, "bootstrap.R"))
source_library(lib_dir)

if (!tpe_available()) {
  stop("This experiment needs `reticulate` + an importable `optuna`. ",
       "See RUNNING.md for setup; nothing to do without TPE.")
}

# --- Configuration -------------------------------------------------------------
# Same protocol as the main benchmark suite (matched budget/reps/seed_start),
# so this sensitivity result is directly comparable to Experiment.tex's
# headline figures. Overridable the same way: --key=value.
sensitivity_default_config <- function() {
  cfg <- default_config()
  cfg$budget     <- 80
  cfg$reps       <- 25
  cfg$seed_start <- 1001L
  cfg$out_dir    <- file.path("results", "tpe_sensitivity")
  cfg
}
cfg_base <- parse_cli_args(commandArgs(trailingOnly = TRUE), sensitivity_default_config())

plan(multisession, workers = max(1L, parallel::detectCores() - 1L))

# --- The gamma grid -------------------------------------------------------------
# gamma(n) returns the number of "good" trials out of n completed ones; a
# quantile-based callable is the simplest way to vary it. 0.25 sits close to
# Optuna's own default behaviour, giving a built-in sanity check.
gamma_quantile <- function(q) function(n) as.integer(ceiling(q * max(n, 1)))
gamma_grid     <- c(0.10, 0.25, 0.50, 0.75)

make_configs <- function() {
  configs <- lapply(gamma_grid, function(q) {
    list(sampler_opts = list(gamma = gamma_quantile(q)))
  })
  names(configs) <- sprintf("TPE (gamma=%.2f)", gamma_grid)
  configs
}

# --- One objective: reference curves (BASS-BO/GP-BO/Random) + the gamma sweep -
run_one_objective <- function(cfg) {
  objective <- load_objective(cfg$objective, cfg$d, cfg$cat_L)
  cat(sprintf("\n== %s (d=%d) | budget=%d | reps=%d ==\n",
              cfg$objective, objective$d, cfg$budget, cfg$reps))

  cat("Running BASS-BO / GP-BO / Random reference curves ...\n")
  reference_runs <- run_experiment(cfg)

  cat("Running TPE gamma sweep ...\n")
  sweep_runs <- run_tpe_sweep_experiment(cfg, make_configs())

  all_runs <- dplyr::bind_rows(reference_runs, sweep_runs)
  final_summary <- save_results(all_runs, objective, cfg)

  # Rank-volatility check: how much does TPE's final performance move across
  # gamma, relative to the fixed (single-curve) BASS-BO/GP-BO baselines?
  cat("\nFinal best-so-far by method (sorted):\n")
  print(final_summary)

  tpe_rows <- dplyr::filter(final_summary, grepl("^TPE", method))
  cat(sprintf(
    "\nTPE final-best range across gamma: [%.4f, %.4f] (spread = %.4f)\n",
    min(tpe_rows$mean_final), max(tpe_rows$mean_final),
    max(tpe_rows$mean_final) - min(tpe_rows$mean_final)
  ))

  final_summary
}

# --- Run on one continuous and one categorical benchmark ----------------------
cfg_branin <- cfg_base
cfg_branin$objective <- "branin"
cfg_branin$d         <- 2
cfg_branin$out_dir   <- file.path(cfg_base$out_dir, "branin")

cfg_catackley <- cfg_base
cfg_catackley$objective <- "cat_ackley"
cfg_catackley$d         <- 6
cfg_catackley$out_dir   <- file.path(cfg_base$out_dir, "cat_ackley")

run_one_objective(cfg_branin)
run_one_objective(cfg_catackley)

cat(sprintf("\nArtifacts saved under: %s\n", normalizePath(cfg_base$out_dir)))

# --- Shut down parallel workers so the process can exit -----------------------
plan(sequential)
