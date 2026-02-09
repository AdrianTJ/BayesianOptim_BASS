#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lhs)
  library(BASS)
  library(GPfit)
})

# =========================
# Objective function
# =========================
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

# =========================
# Utilities
# =========================
is_duplicate <- function(x, X, tol = 1e-10) {
  X <- as.matrix(X)
  xmat <- matrix(x, nrow = nrow(X), ncol = ncol(X), byrow = TRUE)
  any(rowSums((X - xmat)^2) <= tol^2)
}

# =========================
# Optimizers
# =========================
run_bass_bo <- function(X_init, y_init, budget, n_cand, kappa, dup_tol = 1e-10, verbose = FALSE) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
    fit <- bass(xx = X_eval, y = y_eval, degree = 2, verbose = FALSE)
    X_cand <- maximinLHS(n_cand, d)
    
    pred <- predict(fit, newdata = as.data.frame(X_cand))
    pred_mat <- as.matrix(pred)
    if (ncol(pred_mat) != nrow(X_cand) && nrow(pred_mat) == nrow(X_cand)) pred_mat <- t(pred_mat)
    
    mu <- colMeans(pred_mat)
    sd_post <- apply(pred_mat, 2, sd)
    lcb <- mu - kappa * sd_post
    
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
    
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- f(x_next)
    
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
    
    if (verbose) cat(sprintf("[BASS] iter %d/%d | best=%.6f\n", t, budget, best_so_far[t + 1]))
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best_so_far = best_so_far)
}

run_gp_bo <- function(X_init, y_init, budget, n_cand, kappa, eps = 1e-10, dup_tol = 1e-10, verbose = FALSE) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
    fit <- GP_fit(X = X_eval, Y = y_eval)
    X_cand <- maximinLHS(n_cand, d)
    
    pred <- predict(object = fit, xnew = X_cand)
    mu <- as.numeric(pred$Y_hat)
    sd_gp <- sqrt(pmax(as.numeric(pred$MSE), 0) + eps)
    lcb <- mu - kappa * sd_gp
    
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
    
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- f(x_next)
    
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
    
    if (verbose) cat(sprintf("[GP]   iter %d/%d | best=%.6f\n", t, budget, best_so_far[t + 1]))
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best_so_far = best_so_far)
}

run_random_search <- function(X_init, y_init, budget, dup_tol = 1e-10, verbose = FALSE) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best_so_far <- numeric(budget + 1)
  best_so_far[1] <- min(y_eval)
  
  for (t in 1:budget) {
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

# =========================
# One replicate
# =========================
run_one_seed <- function(seed,
                         d = 3,
                         n0 = max(2 * d + 1, 8),
                         budget = 80,
                         n_cand = 1000,
                         kappa = 2.0,
                         eps = 1e-10,
                         dup_tol = 1e-10,
                         verbose = FALSE) {
  set.seed(seed)
  
  X_init <- maximinLHS(n0, d)
  y_init <- f(X_init)
  
  res_bass <- run_bass_bo(X_init, y_init, budget, n_cand, kappa, dup_tol, verbose)
  res_gp   <- run_gp_bo(X_init, y_init, budget, n_cand, kappa, eps, dup_tol, verbose)
  res_rand <- run_random_search(X_init, y_init, budget, dup_tol, verbose)
  
  bind_rows(
    tibble(seed = seed, iter = 0:budget, method = "BASS-BO", best = res_bass$best_so_far),
    tibble(seed = seed, iter = 0:budget, method = "GP-BO", method_order = 2, best = res_gp$best_so_far),
    tibble(seed = seed, iter = 0:budget, method = "Random Search", best = res_rand$best_so_far)
  ) %>%
    select(seed, iter, method, best)
}

# =========================
# Multi-seed experiment
# =========================
run_experiment <- function(seeds,
                           d = 3,
                           budget = 80,
                           n_cand = 1000,
                           kappa = 2.0,
                           eps = 1e-10,
                           dup_tol = 1e-10,
                           verbose = FALSE) {
  n0 <- max(2 * d + 1, 8)
  
  all_runs <- map_dfr(
    seeds,
    ~ run_one_seed(
      seed = .x, d = d, n0 = n0, budget = budget, n_cand = n_cand,
      kappa = kappa, eps = eps, dup_tol = dup_tol, verbose = verbose
    )
  )
  
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

# =========================
# Main (CLI-friendly)
# =========================
args <- commandArgs(trailingOnly = TRUE)

# Defaults
target_name <- "branin"
n_reps <- 10
seed_start <- 1001
d <- 3
budget <- 80
n_cand <- 1000
kappa <- 2.0
verbose <- FALSE
out_dir <- "results"

# Parse simple --key=value args
for (a in args) {
  if (grepl("^--reps=", a)) n_reps <- as.integer(sub("^--reps=", "", a))
  if (grepl("^--seed_start=", a)) seed_start <- as.integer(sub("^--seed_start=", "", a))
  if (grepl("^--d=", a)) d <- as.integer(sub("^--d=", "", a))
  if (grepl("^--budget=", a)) budget <- as.integer(sub("^--budget=", "", a))
  if (grepl("^--n_cand=", a)) n_cand <- as.integer(sub("^--n_cand=", "", a))
  if (grepl("^--kappa=", a)) kappa <- as.numeric(sub("^--kappa=", "", a))
  if (grepl("^--verbose=", a)) verbose <- tolower(sub("^--verbose=", "", a)) %in% c("1","true","t","yes","y")
  if (grepl("^--out_dir=", a)) out_dir <- sub("^--out_dir=", "", a)
  if (grepl("^--target=", a)) target_name <- sub("^--target=", "", a)
  
}

seeds <- seed_start + 0:(n_reps - 1)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Running %d replicates | d=%d | budget=%d | n_cand=%d | kappa=%.3f\n",
            n_reps, d, budget, n_cand, kappa))

res <- run_experiment(
  seeds = seeds,
  d = d,
  budget = budget,
  n_cand = n_cand,
  kappa = kappa,
  verbose = verbose
)

# Save outputs
write_csv(res$all_runs, file.path(out_dir, "all_runs.csv"))
write_csv(res$summary_curve, file.path(out_dir, "summary_curve.csv"))
write_csv(res$final_summary, file.path(out_dir, "final_summary.csv"))

# Plot mean curve + 95% CI
p <- ggplot(res$summary_curve, aes(x = iter, y = mean_best, color = method, fill = method)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) +
  labs(
    title = sprintf("Mean Convergence Across %d Seeds (d=%d)", n_reps, d),
    x = "Iteration (after initialization)",
    y = "Best objective so far (lower is better)"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "convergence_mean_ci.png"), p, width = 8, height = 4, dpi = 150)

print(res$final_summary)
cat(sprintf("\nSaved files in: %s\n", normalizePath(out_dir)))