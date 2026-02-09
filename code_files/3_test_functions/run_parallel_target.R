#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lhs)
  library(BASS)
  library(GPfit)
  library(future)
  library(furrr)
  library(ggplot2)
})

# =========================
# Parallel setup
# =========================
n_workers <- max(1L, parallel::detectCores() - 1L)
plan(multisession, workers = n_workers)

# =========================
# CLI arguments
# =========================
args <- commandArgs(trailingOnly = TRUE)

n_reps <- 10
seed_start <- 1001
d <- 2
budget <- 80
n_cand <- 1000
kappa <- 2.0
eps <- 1e-10
verbose <- FALSE
out_dir <- "results"
target_name <- "branin"

# BASS defaults
bass_kappa_start <- 3.5
bass_kappa_end <- 1.5
bass_sd_floor <- 1e-3
bass_sd_inflate <- 1.20
bass_local_frac <- 0.35
bass_local_sd <- 0.08
bass_explore_every <- 7
bass_degree_early <- 1
bass_degree_late <- 2
bass_switch_after <- 40
bass_print_every <- 10

for (a in args) {
  if (grepl("^--reps=", a)) n_reps <- as.integer(sub("^--reps=", "", a))
  if (grepl("^--seed_start=", a)) seed_start <- as.integer(sub("^--seed_start=", "", a))
  if (grepl("^--d=", a)) d <- as.integer(sub("^--d=", "", a))
  if (grepl("^--budget=", a)) budget <- as.integer(sub("^--budget=", "", a))
  if (grepl("^--n_cand=", a)) n_cand <- as.integer(sub("^--n_cand=", "", a))
  if (grepl("^--kappa=", a)) kappa <- as.numeric(sub("^--kappa=", "", a))
  if (grepl("^--verbose=", a))
    verbose <- tolower(sub("^--verbose=", "", a)) %in% c("1","true","t","yes","y")
  if (grepl("^--out_dir=", a)) out_dir <- sub("^--out_dir=", "", a)
  if (grepl("^--target=", a)) target_name <- sub("^--target=", "", a)
}

# =========================
# Load target
# =========================
source("targets/target_utils.R", local = TRUE)
source(file.path("targets", paste0(target_name, ".R")), local = TRUE)

target_fn     <- get(target_name, inherits = TRUE)
target_bounds <- get(paste0(target_name, "_bounds"), inherits = TRUE)

if (d != length(target_bounds$lower))
  stop("Dimension mismatch with target")

f <- vectorize_target(target_fn, target_bounds)

# =========================
# Utilities
# =========================
is_duplicate <- function(x, X, tol = 1e-10) {
  x <- as.numeric(x)
  X <- as.matrix(X)
  any(rowSums((X - matrix(x, nrow(X), ncol(X), byrow = TRUE))^2) <= tol^2)
}

# =========================
# Optimizers
# =========================
run_bass_bo <- function(X_init, y_init) {
  X_eval <- X_init
  y_eval <- y_init
  d <- ncol(X_eval)
  
  best <- numeric(budget + 1)
  best[1] <- min(y_eval)
  
  for (t in 1:budget) {
    y_mean <- mean(y_eval)
    y_sd <- sd(y_eval); if (!is.finite(y_sd) || y_sd < 1e-12) y_sd <- 1
    y_std <- (y_eval - y_mean) / y_sd
    
    deg <- if (nrow(X_eval) < bass_switch_after) bass_degree_early else bass_degree_late
    fit <- bass(xx = X_eval, y = y_std, degree = deg, verbose = FALSE)
    
    kappa_t <- bass_kappa_start +
      (bass_kappa_end - bass_kappa_start) * (t - 1) / max(1, budget - 1)
    
    n_local <- max(1L, round(n_cand * bass_local_frac))
    n_global <- n_cand - n_local
    
    X_cand <- rbind(
      maximinLHS(n_global, d),
      pmin(pmax(matrix(
        rnorm(n_local * d,
              mean = rep(X_eval[which.min(y_eval), ], each = n_local),
              sd = bass_local_sd),
        ncol = d
      ), 0), 1)
    )
    
    pred <- predict(fit, newdata = as.data.frame(X_cand))
    pred_mat <- as.matrix(pred)
    if (ncol(pred_mat) != nrow(X_cand)) pred_mat <- t(pred_mat)
    
    mu <- colMeans(pred_mat) * y_sd + y_mean
    sd_post <- apply(pred_mat, 2, sd) * y_sd
    sd_post <- pmax(sd_post * bass_sd_inflate, bass_sd_floor)
    
    ord <- if (t %% bass_explore_every == 0)
      order(sd_post, decreasing = TRUE) else order(mu - kappa_t * sd_post)
    
    pick <- ord[!sapply(ord, function(i)
      is_duplicate(X_cand[i, ], X_eval))][1]
    if (is.na(pick)) pick <- ord[1]
    
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- f(x_next)
    
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best[t + 1] <- min(y_eval)
  }
  
  best
}

run_gp_bo <- function(X_init, y_init) {
  X_eval <- X_init
  y_eval <- y_init
  best <- numeric(budget + 1)
  best[1] <- min(y_eval)
  
  for (t in 1:budget) {
    fit <- GP_fit(X_eval, y_eval)
    X_cand <- maximinLHS(n_cand, ncol(X_eval))
    p <- predict(fit, X_cand)
    lcb <- p$Y_hat - kappa * sqrt(pmax(p$MSE, 0) + eps)
    
    ord <- order(lcb)
    pick <- ord[!sapply(ord, function(i)
      is_duplicate(X_cand[i, ], X_eval))][1]
    if (is.na(pick)) pick <- ord[1]
    
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- f(x_next)
    
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best[t + 1] <- min(y_eval)
  }
  
  best
}

run_random_search <- function(X_init, y_init) {
  X_eval <- X_init
  y_eval <- y_init
  best <- numeric(budget + 1)
  best[1] <- min(y_eval)
  
  for (t in 1:budget) {
    repeat {
      x <- matrix(runif(ncol(X_eval)), 1)
      if (!is_duplicate(x, X_eval)) break
    }
    y <- f(x)
    X_eval <- rbind(X_eval, x)
    y_eval <- c(y_eval, y)
    best[t + 1] <- min(y_eval)
  }
  
  best
}

# =========================
# One seed
# =========================
run_one_seed <- function(seed) {
  set.seed(seed)
  n0 <- max(2 * d + 1, 8)
  X_init <- maximinLHS(n0, d)
  y_init <- f(X_init)
  
  tibble(
    seed = seed,
    iter = 0:budget,
    `BASS-BO` = run_bass_bo(X_init, y_init),
    `GP-BO` = run_gp_bo(X_init, y_init),
    `Random Search` = run_random_search(X_init, y_init)
  ) %>%
    pivot_longer(-c(seed, iter), names_to = "method", values_to = "best")
}

# =========================
# Run (parallel)
# =========================
seeds <- seed_start + 0:(n_reps - 1)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)



all_runs <- future_map_dfr(
  seeds,
  run_one_seed,
  .options = furrr_options(seed = TRUE)
)

# =========================
# Summaries
# =========================

summary_curve <- all_runs %>%
  group_by(method, iter) %>%
  summarize(
    mean_best = mean(best),
    sd_best   = sd(best),
    n         = n(),
    .groups   = "drop"
  ) %>%
  mutate(
    se = sd_best / sqrt(n),
    ci_low  = mean_best - 1.96 * se,
    ci_high = mean_best + 1.96 * se
  )

final_summary <- all_runs %>%
  group_by(seed, method) %>%
  filter(iter == max(iter)) %>%
  ungroup() %>%
  group_by(method) %>%
  summarize(
    mean_final = mean(best),
    sd_final   = sd(best),
    .groups = "drop"
  ) %>%
  arrange(mean_final)


write_csv(all_runs, file.path(out_dir, "all_runs.csv"))
write_csv(summary_curve, file.path(out_dir, "summary_curve.csv"))
write_csv(final_summary, file.path(out_dir, "final_summary.csv"))



p <- ggplot(
  summary_curve,
  aes(x = iter, y = mean_best, color = method, fill = method)
) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) +
  labs(
    title = sprintf("Mean Convergence Across %d Seeds (d=%d)", n_reps, d),
    x = "Iteration (after initialization)",
    y = "Best objective so far (lower is better)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "convergence_mean_ci.png"),
  plot = p,
  width = 8,
  height = 4,
  dpi = 150
)


