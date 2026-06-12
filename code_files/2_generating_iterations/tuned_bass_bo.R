#!/usr/bin/env Rscript

#' Advanced Bayesian Optimization with Tuned BASS
#' 
#' This script implements a highly tuned version of Bayesian Optimization using 
#' Bayesian Adaptive Spline Surfaces (BASS). It features:
#' - Parallel execution across multiple seeds.
#' - Dynamic exploration/exploitation (kappa decay).
#' - Hybrid candidate sampling (global + local).
#' - Adaptive surrogate complexity (degree switching).
#'
#' Author: Adrian TJ
#' Date: June 2026

suppressPackageStartupMessages({
  library(tidyverse)
  library(lhs)
  library(BASS)
  library(GPfit)
  library(future)
  library(furrr)
})

# ==============================================================================
# Parallel Infrastructure
# ==============================================================================
n_workers <- max(1L, parallel::detectCores() - 1L)
plan(multisession, workers = n_workers)

# ==============================================================================
# Objective Function
# ==============================================================================

#' Complex Synthetic Objective Function
#'
#' Features multiple local minima, a global trend, and a jump discontinuity.
f <- function(X) {
  X <- as.matrix(X)
  if (is.null(nrow(X))) X <- matrix(X, nrow = 1)
  
  base <- rowSums((X - 0.35)^2)
  osc1 <- sin(6 * pi * X[, 1]) * exp(-2 * X[, 1])
  osc2 <- if (ncol(X) >= 2) 0.35 * cos(5 * pi * X[, 2]^2) else 0
  inter <- if (ncol(X) >= 2) 0.5 * X[, 1] * X[, 2] else 0
  
  c1 <- rep(0.2, ncol(X))
  c2 <- rep(0.75, ncol(X))
  bump <- 1.2 * exp(-35 * rowSums((X - matrix(c1, nrow(X), ncol(X), byrow = TRUE))^2))
  valley <- -1.0 * exp(-28 * rowSums((X - matrix(c2, nrow(X), ncol(X), byrow = TRUE))^2))
  
  jump <- ifelse(X[, 1] > 0.62, 0.4, 0)
  
  base + osc1 + osc2 + inter + bump + valley + jump
}

# ==============================================================================
# Optimization Utilities
# ==============================================================================

#' Check for Duplicate Candidate Points
is_duplicate <- function(x, X, tol = 1e-10) {
  X <- as.matrix(X)
  xmat <- matrix(x, nrow = nrow(X), ncol = ncol(X), byrow = TRUE)
  any(rowSums((X - xmat)^2) <= tol^2)
}

# ==============================================================================
# Optimization Algorithms
# ==============================================================================

#' Tuned BASS Bayesian Optimization
#'
#' Implements advanced heuristics for uncertainty inflation and local refinement.
#'
#' @param kappa_start Initial LCB exploration weight.
#' @param kappa_end Final LCB exploration weight (linear decay).
#' @param sd_inflate Multiplier to prevent overconfidence in surrogate.
#' @param local_frac Proportion of candidates sampled near current best.
#' @param switch_after Iteration to switch from linear (degree 1) to additive (degree 2) splines.
run_bass_bo <- function(
    X_init, y_init, budget, n_cand = 4000,
    kappa_start = 3.5, kappa_end = 1.5,
    dup_tol = 1e-10, sd_floor = 1e-3, sd_inflate = 1.20,
    local_frac = 0.35, local_sd = 0.08,
    explore_every = 7,
    degree_early = 1, degree_late = 2, switch_after = 40,
    verbose = FALSE, print_every = 10
) {
  
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
    # Standardize target for numerical stability in BASS
    y_mean <- mean(y_eval)
    y_sd <- sd(y_eval)
    if (!is.finite(y_sd) || y_sd < 1e-12) y_sd <- 1
    y_std <- (y_eval - y_mean) / y_sd
    
    # Increase model complexity as more data becomes available
    deg <- if (nrow(X_eval) < switch_after) degree_early else degree_late
    fit <- bass(xx = X_eval, y = y_std, degree = deg, verbose = FALSE)
    
    # Linear decay for exploration parameter
    kappa_t <- kappa_start + (kappa_end - kappa_start) * (t - 1) / max(1, budget - 1)
    
    # Hybrid Candidate Sampling
    n_local <- max(1L, round(n_cand * local_frac))
    n_global <- n_cand - n_local
    X_global <- maximinLHS(n_global, d)
    x_best <- X_eval[which.min(y_eval), ]
    
    # Local refinement around current optimum
    X_local <- matrix(
      rnorm(n_local * d, mean = rep(x_best, each = n_local), sd = local_sd),
      ncol = d, byrow = FALSE
    )
    X_local <- pmin(pmax(X_local, 0), 1)
    
    X_cand <- rbind(X_global, X_local)
    
    # Prediction and Acquisition
    pred <- predict(fit, newdata = as.data.frame(X_cand))
    pred_mat <- as.matrix(pred)
    if (ncol(pred_mat) != nrow(X_cand)) pred_mat <- t(pred_mat)
    
    mu <- colMeans(pred_mat) * y_sd + y_mean
    sd_post <- apply(pred_mat, 2, sd) * y_sd
    sd_post <- pmax(sd_post * sd_inflate, sd_floor) # Inflate SD to prevent premature convergence
    
    lcb <- mu - kappa_t * sd_post
    
    # Periodic Pure Exploration step
    ord <- if (!is.null(explore_every) && t %% explore_every == 0)
      order(sd_post, decreasing = TRUE) else order(lcb)
    
    pick <- ord[!sapply(ord, function(i)
      is_duplicate(X_cand[i, ], X_eval, dup_tol))][1]
    
    if (is.na(pick)) pick <- ord[1]
    
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- f(x_next)
    
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best_so_far = best_so_far)
}

#' Standard GP-BO Baseline
run_gp_bo <- function(X_init, y_init, budget, n_cand, kappa, eps, dup_tol, verbose = FALSE) {
  X_eval <- X_init
  y_eval <- y_init
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
    fit <- GP_fit(X_eval, y_eval)
    X_cand <- maximinLHS(n_cand, d)
    
    pred <- predict(fit, X_cand)
    mu <- pred$Y_hat
    sd <- sqrt(pmax(pred$MSE, 0) + eps)
    lcb <- mu - kappa * sd
    
    ord <- order(lcb)
    pick <- ord[!sapply(ord, function(i)
      is_duplicate(X_cand[i, ], X_eval, dup_tol))][1]
    
    if (is.na(pick)) pick <- ord[1]
    
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- f(x_next)
    
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
  }
  
  list(best_so_far = best_so_far)
}

#' Pure Random Search Baseline
run_random_search <- function(X_init, y_init, budget, dup_tol, verbose = FALSE) {
  X_eval <- X_init
  y_eval <- y_init
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
    repeat {
      x_next <- matrix(runif(d), 1)
      if (!is_duplicate(x_next, X_eval, dup_tol)) break
    }
    y_next <- f(x_next)
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
  }
  
  list(best_so_far = best_so_far)
}

# ==============================================================================
# Replicated Experiment Harness
# ==============================================================================

#' Run Single Seed Simulation
run_one_seed <- function(seed, d, n0, budget, n_cand, kappa, eps, dup_tol, ...) {
  set.seed(seed)
  X_init <- maximinLHS(n0, d)
  y_init <- f(X_init)
  
  bass <- run_bass_bo(X_init, y_init, budget, n_cand, ...)
  gp   <- run_gp_bo(X_init, y_init, budget, n_cand, kappa, eps, dup_tol)
  rnd  <- run_random_search(X_init, y_init, budget, dup_tol)
  
  bind_rows(
    tibble(seed, iter = 0:budget, method = "BASS-BO", best = bass$best_so_far),
    tibble(seed, iter = 0:budget, method = "GP-BO",   best = gp$best_so_far),
    tibble(seed, iter = 0:budget, method = "Random",  best = rnd$best_so_far)
  )
}

#' Run Experiment in Parallel
run_experiment <- function(seeds, d, budget, n_cand, kappa, eps, dup_tol, ...) {
  n0 <- max(2 * d + 1, 8)
  
  future_map_dfr(
    seeds,
    ~ run_one_seed(.x, d, n0, budget, n_cand, kappa, eps, dup_tol, ...),
    .options = furrr_options(seed = TRUE)
  )
}
