#!/usr/bin/env Rscript

# =============================================================================
# run_benchmark.R  --  Single entry point for the synthetic benchmarks
# =============================================================================
# Replaces the three near-identical synthetic runners. It compares BASS-BO,
# GP-BO and Random Search on one objective and writes the CSVs + convergence
# plot. Everything is configurable from the command line; see default_config()
# in R/config.R for the full list of --key=value flags.
#
# Examples:
#   Rscript code_files/run_benchmark.R --objective=branin --d=2 --budget=80 --reps=25
#   Rscript code_files/run_benchmark.R --objective=rastrigin --d=4 --reps=5
#   Rscript code_files/run_benchmark.R --objective=synthetic --d=3 --out_dir=results_syn
#   # Categorical / mixed benchmarks (d is fixed for func2C/func3C):
#   Rscript code_files/run_benchmark.R --objective=func2C --budget=60 --out_dir=results_func2C
#   Rscript code_files/run_benchmark.R --objective=func3C --budget=80 --out_dir=results_func3C
#   Rscript code_files/run_benchmark.R --objective=cat_ackley --d=6 --out_dir=results_catackley
#   # Add the TPE (Optuna) baseline (needs reticulate + an importable optuna):
#   Rscript code_files/run_benchmark.R --objective=func2C --with_tpe=true
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lhs)
  library(BASS)
  library(GPfit)
  library(future)
  library(furrr)
})

# --- Locate and load the R/ library, relative to this script's location -------
# Works no matter which directory the script is launched from.
this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE))
script_dir <- if (length(this_file)) dirname(normalizePath(this_file)) else getwd()
lib_dir <- file.path(script_dir, "R")

source(file.path(lib_dir, "bootstrap.R"))
source_library(lib_dir)

# --- Configuration ------------------------------------------------------------
cfg <- parse_cli_args(commandArgs(trailingOnly = TRUE))

# --- Parallel backend: one worker per core, leaving one free -----------------
plan(multisession, workers = max(1L, parallel::detectCores() - 1L))

# --- Run the experiment -------------------------------------------------------
# The objective and methods are rebuilt from `cfg` inside each parallel worker
# (see run_one_seed), so we only build a copy here for the result labels/plots.
objective <- load_objective(cfg$objective, cfg$d)

cat(sprintf("Running %s (d=%d) | budget=%d | reps=%d\n",
            cfg$objective, objective$d, cfg$budget, cfg$reps))

all_runs <- run_experiment(cfg)

# Optional TPE (Optuna) baseline, run in this process (see R/tpe.R). Added after
# the parallel R methods so a missing Python/Optuna setup never affects them.
if (isTRUE(cfg$with_tpe)) {
  if (tpe_available()) {
    cat("Adding TPE (Optuna) baseline ...\n")
    all_runs <- dplyr::bind_rows(all_runs, run_tpe_experiment(cfg))
  } else {
    warning("--with_tpe=TRUE but `reticulate`/`optuna` are unavailable; ",
            "skipping the TPE baseline. See RUNNING.md for setup.")
  }
}

final_summary <- save_results(all_runs, objective, cfg)

# --- Report -------------------------------------------------------------------
print(final_summary)
cat(sprintf("\nArtifacts saved in: %s\n", normalizePath(cfg$out_dir)))

# --- Shut down parallel workers so the process can exit -----------------------
# plan(multisession) leaves persistent background R sessions running; resetting
# to sequential stops them, otherwise Rscript can hang at exit on the open
# worker connections.
plan(sequential)
