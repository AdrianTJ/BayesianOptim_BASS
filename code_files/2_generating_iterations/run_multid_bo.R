#!/usr/bin/env Rscript

#' Bayesian Optimization Benchmarking Script (Multi-Dimensional)
#' 
#' This script compares three optimization strategies on a synthetic multi-dimensional function:
#' 1. BASS-BO: Bayesian Adaptive Spline Surfaces for Bayesian Optimization
#' 2. GP-BO: Gaussian Process Bayesian Optimization
#' 3. Random Search: Purely stochastic sampling
#'
#' The script supports CLI arguments for batch execution and parallel-friendly design.
#'
#' Author: Adrian TJ
#' Date: June 2026

suppressPackageStartupMessages({
  library(tidyverse)
  library(lhs)
  library(BASS)
  library(GPfit)
})

# ==============================================================================
# Objective Function Definition
# ==============================================================================

#' Synthetic Multi-Dimensional Objective Function
#'
#' A complex, non-linear surface with oscillations, bumps, valleys, and a jump 
#' discontinuity in the first dimension. Designed to challenge surrogate models.
#'
#' @param X A matrix or vector of inputs in [0, 1]^d.
#' @return A numeric vector of function values.
f <- function(X) {
  X <- as.matrix(X)
  if (is.null(nrow(X))) X <- matrix(X, nrow = 1)
  
  # Quadratic base
  base <- rowSums((X - 0.35)^2)
  
  # Oscillations
  osc1 <- sin(6 * pi * X[, 1]) * exp(-2 * X[, 1])
  osc2 <- if (ncol(X) >= 2) 0.35 * cos(5 * pi * X[, 2]^2) else 0
  
  # Interactions
  inter <- if (ncol(X) >= 2) 0.5 * X[, 1] * X[, 2] else 0
  
  # Local features (Bumps and Valleys)
  c1 <- rep(0.2, ncol(X))
  c2 <- rep(0.75, ncol(X))
  bump <- 1.2 * exp(-35 * rowSums((X - matrix(c1, nrow(X), ncol(X), byrow = TRUE))^2))
  valley <- -1.0 * exp(-28 * rowSums((X - matrix(c2, nrow(X), ncol(X), byrow = TRUE))^2))
  
  # Jump discontinuity
  jump <- ifelse(X[, 1] > 0.62, 0.4, 0)
  
  base + osc1 + osc2 + inter + bump + valley + jump
}

# ==============================================================================
# Utility Functions
# ==============================================================================

#' Check for Duplicate Candidates
#'
#' Prevents re-evaluating points that are already in the evaluation set within 
#' a specified Euclidean tolerance.
#'
#' @param x Candidate point.
#' @param X Matrix of previously evaluated points.
#' @param tol Euclidean distance tolerance.
#' @return Logical; TRUE if duplicate exists.
is_duplicate <- function(x, X, tol = 1e-10) {
  X <- as.matrix(X)
  xmat <- matrix(x, nrow = nrow(X), ncol = ncol(X), byrow = TRUE)
  any(rowSums((X - xmat)^2) <= tol^2)
}

# ==============================================================================
# Optimization Algorithms
# ==============================================================================

#' BASS-based Bayesian Optimization
#'
#' @param X_init Initial design matrix.
#' @param y_init Initial function values.
#' @param budget Number of sequential evaluations.
#' @param n_cand Number of candidates to sample at each step.
#' @param kappa Lower Confidence Bound (LCB) exploration parameter.
#' @param dup_tol Duplicate tolerance.
#' @param verbose Print iteration progress.
run_bass_bo <- function(X_init, y_init, budget, n_cand, kappa, dup_tol = 1e-10, verbose = FALSE) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
    # Fit BASS surrogate (adaptive splines)
    fit <- bass(xx = X_eval, y = y_eval, degree = 2, verbose = FALSE)
    
    # Generate space-filling candidates
    X_cand <- maximinLHS(n_cand, d)
    
    # Predict posterior distribution (MCMC draws)
    pred <- predict(fit, newdata = as.data.frame(X_cand))
    pred_mat <- as.matrix(pred)
    # Ensure standard orientation: rows=draws, cols=candidates
    if (ncol(pred_mat) != nrow(X_cand) && nrow(pred_mat) == nrow(X_cand)) pred_mat <- t(pred_mat)
    
    # Compute Acquisition Function: Lower Confidence Bound (LCB)
    mu <- colMeans(pred_mat)
    sd_post <- apply(pred_mat, 2, sd)
    lcb <- mu - kappa * sd_post
    
    # Select best non-duplicate candidate
    ord <- order(lcb)
    pick <- NA_integer_
    for (idx in ord) {
      x_try <- X_cand[idx, ]
      if (!is_duplicate(x_try, X_eval, tol = dup_tol)) {
        pick <- idx
        break
      }
    }
    if (is.na(pick)) pick <- ord[1]
    
    # Evaluate objective
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- f(x_next)
    
    # Update state
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
    
    if (verbose) cat(sprintf("[BASS] iter %d/%d | best=%.6f\n", t, budget, best_so_far[t + 1]))
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best_so_far = best_so_far)
}

#' Gaussian Process Bayesian Optimization
#'
#' @param X_init Initial design matrix.
#' @param y_init Initial function values.
#' @param budget Number of sequential evaluations.
#' @param n_cand Number of candidates to sample at each step.
#' @param kappa LCB exploration parameter.
#' @param eps Small nugget for numerical stability in SD calculation.
#' @param dup_tol Duplicate tolerance.
run_gp_bo <- function(X_init, y_init, budget, n_cand, kappa, eps = 1e-10, dup_tol = 1e-10, verbose = FALSE) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
    # Fit GP surrogate
    fit <- GP_fit(X = X_eval, Y = y_eval)
    X_cand <- maximinLHS(n_cand, d)
    
    # Predict mean and variance
    pred <- predict(object = fit, xnew = X_cand)
    mu <- as.numeric(pred$Y_hat)
    sd_gp <- sqrt(pmax(as.numeric(pred$MSE), 0) + eps)
    
    # Compute Acquisition Function (LCB)
    lcb <- mu - kappa * sd_gp
    
    # Select best non-duplicate candidate
    ord <- order(lcb)
    pick <- NA_integer_
    for (idx in ord) {
      x_try <- X_cand[idx, ]
      if (!is_duplicate(x_try, X_eval, tol = dup_tol)) {
        pick <- idx
        break
      }
    }
    if (is.na(pick)) pick <- ord[1]
    
    # Evaluate objective
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- f(x_next)
    
    # Update state
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
    
    if (verbose) cat(sprintf("[GP]   iter %d/%d | best=%.6f\n", t, budget, best_so_far[t + 1]))
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best_so_far = best_so_far)
}

#' Random Search Baseline
#'
#' @param budget Number of evaluations.
run_random_search <- function(X_init, y_init, budget, dup_tol = 1e-10, verbose = FALSE) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
    # Sample uniformly until a non-duplicate is found
    repeat {
      x_next <- matrix(runif(d), nrow = 1)
      if (!is_duplicate(as.numeric(x_next), X_eval, tol = dup_tol)) break
    }
    
    y_next <- f(x_next)
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
    
    if (verbose) cat(sprintf("[RAND] iter %d/%d | best=%.6f\n", t, budget, best_so_far[t + 1]))
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best_so_far = best_so_far)
}

# ==============================================================================
# Experiment Execution
# ==============================================================================

#' Run One Full Optimization Experiment (Single Seed)
#'
#' @param seed Random seed for reproducibility.
run_one_seed <- function(seed, d, n0, budget, n_cand, kappa, eps, dup_tol, verbose) {
  set.seed(seed)
  
  # Latin Hypercube Sampling for initialization
  X_init <- maximinLHS(n0, d)
  y_init <- f(X_init)
  
  # Run all three methods
  res_bass <- run_bass_bo(X_init, y_init, budget, n_cand, kappa, dup_tol, verbose)
  res_gp   <- run_gp_bo(X_init, y_init, budget, n_cand, kappa, eps, dup_tol, verbose)
  res_rand <- run_random_search(X_init, y_init, budget, dup_tol, verbose)
  
  # Collate results
  bind_rows(
    tibble(seed = seed, iter = 0:budget, method = "BASS-BO", best = res_bass$best_so_far),
    tibble(seed = seed, iter = 0:budget, method = "GP-BO", best = res_gp$best_so_far),
    tibble(seed = seed, iter = 0:budget, method = "Random Search", best = res_rand$best_so_far)
  )
}

#' Run Multi-Seed Experiment and Summarize
#'
#' @param seeds Vector of seeds to run.
run_experiment <- function(seeds, d, budget, n_cand, kappa, eps, dup_tol, verbose) {
  n0 <- max(2 * d + 1, 8)
  
  # Execute replicates sequentially
  all_runs <- map_dfr(
    seeds,
    ~ run_one_seed(
      seed = .x, d = d, n0 = n0, budget = budget, n_cand = n_cand,
      kappa = kappa, eps = eps, dup_tol = dup_tol, verbose = verbose
    )
  )
  
  # Compute mean convergence and 95% CI
  summary_curve <- all_runs %>%
    group_by(method, iter) %>%
    summarize(
      mean_best = mean(best),
      sd_best = sd(best),
      n = n(),
      se = sd_best / sqrt(n),
      ci_low = mean_best - 1.96 * se,
      ci_high = mean_best + 1.96 * se,
      .groups = "drop"
    )
  
  # Final performance metrics
  final_summary <- all_runs %>%
    group_by(seed, method) %>%
    summarize(final_best = best[which.max(iter)], .groups = "drop") %>%
    group_by(method) %>%
    summarize(
      mean_final = mean(final_best),
      sd_final = sd(final_best),
      median_final = median(final_best),
      q25 = quantile(final_best, 0.25),
      q75 = quantile(final_best, 0.75),
      .groups = "drop"
    ) %>%
    arrange(mean_final)
  
  list(all_runs = all_runs, summary_curve = summary_curve, final_summary = final_summary)
}

# ==============================================================================
# CLI Entry Point
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)

# Default configuration
n_reps <- 10
seed_start <- 1001
d <- 3
budget <- 80
n_cand <- 1000
kappa <- 2.0
verbose <- FALSE
out_dir <- "results_multid"

# Parse CLI arguments (--key=value)
for (a in args) {
  if (grepl("^--reps=", a)) n_reps <- as.integer(sub("^--reps=", "", a))
  if (grepl("^--seed_start=", a)) seed_start <- as.integer(sub("^--seed_start=", "", a))
  if (grepl("^--d=", a)) d <- as.integer(sub("^--d=", "", a))
  if (grepl("^--budget=", a)) budget <- as.integer(sub("^--budget=", "", a))
  if (grepl("^--n_cand=", a)) n_cand <- as.integer(sub("^--n_cand=", "", a))
  if (grepl("^--kappa=", a)) kappa <- as.numeric(sub("^--kappa=", "", a))
  if (grepl("^--verbose=", a)) verbose <- tolower(sub("^--verbose=", "", a)) %in% c("1","true","t","yes","y")
  if (grepl("^--out_dir=", a)) out_dir <- sub("^--out_dir=", "", a)
}

# Setup output directory
seeds <- seed_start + 0:(n_reps - 1)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Running %d replicates | d=%d | budget=%d | n_cand=%d | kappa=%.3f\n",
            n_reps, d, budget, n_cand, kappa))

# Execution
res <- run_experiment(
  seeds = seeds,
  d = d,
  budget = budget,
  n_cand = n_cand,
  kappa = kappa,
  verbose = verbose,
  eps = 1e-10,
  dup_tol = 1e-10
)

# Persistence
write_csv(res$all_runs, file.path(out_dir, "all_runs.csv"))
write_csv(res$summary_curve, file.path(out_dir, "summary_curve.csv"))
write_csv(res$final_summary, file.path(out_dir, "final_summary.csv"))

# Visualization
p <- ggplot(res$summary_curve, aes(x = iter, y = mean_best, color = method, fill = method)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) +
  labs(
    title = sprintf("Mean Convergence Across %d Seeds (d=%d)", n_reps, d),
    subtitle = "Comparing BASS-BO, GP-BO, and Random Search",
    x = "Iteration (after initialization)",
    y = "Best objective so far (lower is better)"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "convergence_mean_ci.png"), p, width = 8, height = 4, dpi = 150)

# Console Output
print(res$final_summary)
cat(sprintf("\nSaved all artifacts in: %s\n", normalizePath(out_dir)))
