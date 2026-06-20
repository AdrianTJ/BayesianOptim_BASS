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
            cfg$objective, cfg$d, cfg$budget, cfg$reps))

all_runs      <- run_experiment(cfg)
final_summary <- save_results(all_runs, objective, cfg)

# --- Report -------------------------------------------------------------------
print(final_summary)
cat(sprintf("\nArtifacts saved in: %s\n", normalizePath(cfg$out_dir)))
