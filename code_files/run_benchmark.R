#!/usr/bin/env Rscript

# =============================================================================
# run_benchmark.R  --  Single entry point for the synthetic benchmarks
# =============================================================================
# Replaces the three near-identical synthetic runners. It compares BASS-BO,
# GP-BO and Random Search on one objective and writes the CSVs + convergence
# plot. Everything is configurable from the command line; see default_config()
# in R/config.R for the full list of --key=value flags.
#
# Each run writes into a per-objective subfolder of the results root, e.g.
# results/branin/, results/cat_ackley/, so running different objectives no longer
# overwrites one another. `--out_dir` sets the root (default "results").
#
# Examples:
#   Rscript code_files/run_benchmark.R --objective=branin --d=2 --budget=80 --reps=25
#   Rscript code_files/run_benchmark.R --objective=rastrigin --d=4 --reps=5
#   Rscript code_files/run_benchmark.R --objective=synthetic --d=3
#   # Categorical / mixed benchmarks (d is fixed for func2C/func3C):
#   Rscript code_files/run_benchmark.R --objective=func2C --budget=60
#   Rscript code_files/run_benchmark.R --objective=func3C --budget=80
#   Rscript code_files/run_benchmark.R --objective=cat_ackley --d=6
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

# The objective and methods are rebuilt from `cfg` inside each parallel worker
# (see run_one_seed), so we only build a copy here for the result labels/plots.
objective <- load_objective(cfg$objective, cfg$d, cfg$cat_L)

# Keep the results root tidy: give every objective its own subfolder, so runs on
# different objectives accumulate side by side instead of overwriting each other.
# cat_ackley's difficulty is set by (d, L), so those go into the folder name --
# the easy and hard instances are different benchmarks and must not overwrite
# one another.
run_label <- if (cfg$objective == "cat_ackley") {
  sprintf("cat_ackley_d%d_L%d", objective$d, cfg$cat_L)
} else {
  cfg$objective
}
cfg$out_dir <- file.path(cfg$out_dir, run_label)

# --- Parallel backend: one worker per core, leaving one free -----------------
plan(multisession, workers = max(1L, parallel::detectCores() - 1L))

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

# Paired per-seed comparison vs Random (shared initial designs make the design
# paired; win counts + a signed-rank test are more informative than the means).
paired <- summarise_paired(all_runs)
if (nrow(paired)) {
  cat("\nPaired per-seed comparison vs Random (wins = seeds with a strictly",
      "better final best):\n")
  print(paired)
}

cat(sprintf("\nArtifacts saved in: %s\n", normalizePath(cfg$out_dir)))

# --- Shut down parallel workers so the process can exit -----------------------
# plan(multisession) leaves persistent background R sessions running; resetting
# to sequential stops them, otherwise Rscript can hang at exit on the open
# worker connections.
plan(sequential)
