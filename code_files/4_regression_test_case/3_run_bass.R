#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lhs)
  library(BASS)
  library(GPfit)
  library(glmnet)
  library(MASS)      # Boston dataset
  library(future)
  library(furrr)
})

# =========================
# Parallel setup
# =========================
n_workers <- max(1L, parallel::detectCores() - 1L)
plan(multisession, workers = n_workers)

# =========================
# CLI args
# =========================
args <- commandArgs(trailingOnly = TRUE)

# Core experiment defaults
n_reps <- 10
seed_start <- 1001
budget <- 60
n_cand <- 2000
verbose <- FALSE
out_dir <- "results_enet_bo"

# Data/model defaults
train_frac <- 0.8
nfolds <- 5
lambda_log10_min <- -5
lambda_log10_max <- 1

# GP defaults
gp_kappa <- 2.0
eps <- 1e-10

# BASS tuned defaults
bass_kappa_start <- 3.0
bass_kappa_end <- 1.2
bass_sd_floor <- 1e-4
bass_sd_inflate <- 1.15
bass_local_frac <- 0.35
bass_local_sd <- 0.08
bass_explore_every <- 6
bass_degree_early <- 1
bass_degree_late <- 2
bass_switch_after <- 30
bass_print_every <- 10

for (a in args) {
  if (grepl("^--reps=", a)) n_reps <- as.integer(sub("^--reps=", "", a))
  if (grepl("^--seed_start=", a)) seed_start <- as.integer(sub("^--seed_start=", "", a))
  if (grepl("^--budget=", a)) budget <- as.integer(sub("^--budget=", "", a))
  if (grepl("^--n_cand=", a)) n_cand <- as.integer(sub("^--n_cand=", "", a))
  if (grepl("^--out_dir=", a)) out_dir <- sub("^--out_dir=", "", a)
  if (grepl("^--verbose=", a)) verbose <- tolower(sub("^--verbose=", "", a)) %in% c("1","true","t","yes","y")
  if (grepl("^--nfolds=", a)) nfolds <- as.integer(sub("^--nfolds=", "", a))
  if (grepl("^--gp_kappa=", a)) gp_kappa <- as.numeric(sub("^--gp_kappa=", "", a))
  if (grepl("^--lambda_log10_min=", a)) lambda_log10_min <- as.numeric(sub("^--lambda_log10_min=", "", a))
  if (grepl("^--lambda_log10_max=", a)) lambda_log10_max <- as.numeric(sub("^--lambda_log10_max=", "", a))
  
  # BASS knobs
  if (grepl("^--bass_kappa_start=", a)) bass_kappa_start <- as.numeric(sub("^--bass_kappa_start=", "", a))
  if (grepl("^--bass_kappa_end=", a)) bass_kappa_end <- as.numeric(sub("^--bass_kappa_end=", "", a))
  if (grepl("^--bass_sd_floor=", a)) bass_sd_floor <- as.numeric(sub("^--bass_sd_floor=", "", a))
  if (grepl("^--bass_sd_inflate=", a)) bass_sd_inflate <- as.numeric(sub("^--bass_sd_inflate=", "", a))
  if (grepl("^--bass_local_frac=", a)) bass_local_frac <- as.numeric(sub("^--bass_local_frac=", "", a))
  if (grepl("^--bass_local_sd=", a)) bass_local_sd <- as.numeric(sub("^--bass_local_sd=", "", a))
  if (grepl("^--bass_explore_every=", a)) bass_explore_every <- as.integer(sub("^--bass_explore_every=", "", a))
  if (grepl("^--bass_degree_early=", a)) bass_degree_early <- as.integer(sub("^--bass_degree_early=", "", a))
  if (grepl("^--bass_degree_late=", a)) bass_degree_late <- as.integer(sub("^--bass_degree_late=", "", a))
  if (grepl("^--bass_switch_after=", a)) bass_switch_after <- as.integer(sub("^--bass_switch_after=", "", a))
  if (grepl("^--bass_print_every=", a)) bass_print_every <- as.integer(sub("^--bass_print_every=", "", a))
}

# =========================
# Helpers
# =========================
decode_enet_params <- function(Xu, lmin = -5, lmax = 1) {
  Xu <- as.matrix(Xu)
  if (is.null(nrow(Xu))) Xu <- matrix(Xu, nrow = 1)
  
  u_alpha  <- pmin(pmax(Xu[, 1], 0), 1)
  u_lambda <- pmin(pmax(Xu[, 2], 0), 1)
  
  alpha <- u_alpha
  log10_lambda <- lmin + (lmax - lmin) * u_lambda
  lambda <- 10^log10_lambda
  
  tibble(alpha = alpha, lambda = lambda, log10_lambda = log10_lambda)
}

is_duplicate <- function(x, X, tol = 1e-10) {
  x <- as.numeric(x)
  X <- as.matrix(X)
  any(rowSums((X - matrix(x, nrow(X), ncol(X), byrow = TRUE))^2) <= tol^2)
}

make_folds <- function(n, k = 5, seed = 1L) {
  set.seed(seed)
  sample(rep(1:k, length.out = n))
}

# CV objective (minimize RMSE)
make_enet_objective <- function(x_train, y_train, nfolds, lmin, lmax, seed_offset = 0L) {
  force(x_train); force(y_train); force(nfolds); force(lmin); force(lmax); force(seed_offset)
  
  n <- nrow(x_train)
  fold_id <- make_folds(n, k = nfolds, seed = 123 + seed_offset)
  
  function(Xu) {
    Xu <- as.matrix(Xu)
    if (is.null(nrow(Xu))) Xu <- matrix(Xu, nrow = 1)
    
    pars <- decode_enet_params(Xu, lmin = lmin, lmax = lmax)
    out <- numeric(nrow(pars))
    
    for (i in seq_len(nrow(pars))) {
      a <- pars$alpha[i]
      lam <- pars$lambda[i]
      
      fold_rmse <- numeric(nfolds)
      
      for (k in seq_len(nfolds)) {
        tr <- fold_id != k
        va <- fold_id == k
        
        fit <- glmnet(
          x = x_train[tr, , drop = FALSE],
          y = y_train[tr],
          alpha = a,
          lambda = lam,            # single lambda is valid in glmnet()
          standardize = FALSE
        )
        
        pred <- as.numeric(predict(fit, newx = x_train[va, , drop = FALSE], s = lam))
        fold_rmse[k] <- sqrt(mean((pred - y_train[va])^2))
      }
      
      out[i] <- mean(fold_rmse)
    }
    
    out
  }
}

# =========================
# Optimizers over [0,1]^2
# =========================
run_bass_bo <- function(X_init, y_init, objective_fn) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best <- numeric(budget + 1)
  best[1] <- min(y_eval)
  
  for (t in 1:budget) {
    # Standardize y
    y_mean <- mean(y_eval)
    y_sd <- sd(y_eval); if (!is.finite(y_sd) || y_sd < 1e-12) y_sd <- 1
    y_std <- (y_eval - y_mean) / y_sd
    
    deg <- if (nrow(X_eval) < bass_switch_after) bass_degree_early else bass_degree_late
    fit <- bass(xx = X_eval, y = y_std, degree = deg, verbose = FALSE)
    
    kappa_t <- bass_kappa_start + (bass_kappa_end - bass_kappa_start) * (t - 1) / max(1, budget - 1)
    
    n_local <- max(1L, round(n_cand * bass_local_frac))
    n_global <- n_cand - n_local
    
    X_global <- maximinLHS(n_global, d)
    x_best <- X_eval[which.min(y_eval), ]
    
    X_local <- matrix(
      rnorm(n_local * d, mean = rep(x_best, each = n_local), sd = bass_local_sd),
      ncol = d, byrow = FALSE
    )
    X_local <- pmin(pmax(X_local, 0), 1)
    
    X_cand <- rbind(X_global, X_local)
    
    pred <- predict(fit, newdata = as.data.frame(X_cand))
    pred_mat <- as.matrix(pred)
    if (ncol(pred_mat) != nrow(X_cand) && nrow(pred_mat) == nrow(X_cand)) pred_mat <- t(pred_mat)
    
    mu <- colMeans(pred_mat) * y_sd + y_mean
    sd_post <- apply(pred_mat, 2, sd) * y_sd
    sd_post <- pmax(sd_post * bass_sd_inflate, bass_sd_floor)
    
    ord <- if (!is.null(bass_explore_every) && bass_explore_every > 0 && (t %% bass_explore_every == 0)) {
      order(sd_post, decreasing = TRUE)
    } else {
      order(mu - kappa_t * sd_post)
    }
    
    pick <- ord[!sapply(ord, function(i) is_duplicate(X_cand[i, ], X_eval))][1]
    if (is.na(pick)) pick <- ord[1]
    
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- objective_fn(x_next)
    
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best[t + 1] <- min(y_eval)
    
    if (verbose && (t == 1 || t %% bass_print_every == 0 || t == budget)) {
      cat(sprintf("[BASS] iter %d/%d | best CV RMSE=%.5f\n", t, budget, best[t + 1]))
      flush.console()
    }
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best = best)
}

run_gp_bo <- function(X_init, y_init, objective_fn) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best <- numeric(budget + 1)
  best[1] <- min(y_eval)
  
  for (t in 1:budget) {
    fit <- GP_fit(X = X_eval, Y = y_eval)
    X_cand <- maximinLHS(n_cand, d)
    
    p <- predict(fit, X_cand)
    mu <- as.numeric(p$Y_hat)
    sd_gp <- sqrt(pmax(as.numeric(p$MSE), 0) + eps)
    lcb <- mu - gp_kappa * sd_gp
    
    ord <- order(lcb)
    pick <- ord[!sapply(ord, function(i) is_duplicate(X_cand[i, ], X_eval))][1]
    if (is.na(pick)) pick <- ord[1]
    
    x_next <- X_cand[pick, , drop = FALSE]
    y_next <- objective_fn(x_next)
    
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best[t + 1] <- min(y_eval)
    
    if (verbose && (t == 1 || t %% 10 == 0 || t == budget)) {
      cat(sprintf("[GP]   iter %d/%d | best CV RMSE=%.5f\n", t, budget, best[t + 1]))
      flush.console()
    }
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best = best)
}

run_random_search <- function(X_init, y_init, objective_fn) {
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d <- ncol(X_eval)
  
  best <- numeric(budget + 1)
  best[1] <- min(y_eval)
  
  for (t in 1:budget) {
    repeat {
      x_next <- matrix(runif(d), nrow = 1)
      if (!is_duplicate(x_next, X_eval)) break
    }
    
    y_next <- objective_fn(x_next)
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best[t + 1] <- min(y_eval)
    
    if (verbose && (t == 1 || t %% 10 == 0 || t == budget)) {
      cat(sprintf("[RAND] iter %d/%d | best CV RMSE=%.5f\n", t, budget, best[t + 1]))
      flush.console()
    }
  }
  
  list(X_eval = X_eval, y_eval = y_eval, best = best)
}

# =========================
# One replicate
# =========================
run_one_seed <- function(seed) {
  set.seed(seed)
  
  # Boston split
  data("Boston", package = "MASS")
  df <- as_tibble(Boston)
  
  idx <- sample(seq_len(nrow(df)), size = floor(train_frac * nrow(df)))
  train <- df[idx, ]
  test  <- df[-idx, ]
  
  x_train_raw <- as.matrix(select(train, -medv))
  y_train <- train$medv
  x_test_raw <- as.matrix(select(test, -medv))
  y_test <- test$medv
  
  # Scale predictors with train statistics
  ctr <- colMeans(x_train_raw)
  scl <- apply(x_train_raw, 2, sd)
  scl[scl == 0] <- 1
  
  x_train <- scale(x_train_raw, center = ctr, scale = scl)
  x_test  <- scale(x_test_raw,  center = ctr, scale = scl)
  
  # Objective function for BO (CV RMSE)
  obj <- make_enet_objective(
    x_train = x_train, y_train = y_train,
    nfolds = nfolds, lmin = lambda_log10_min, lmax = lambda_log10_max,
    seed_offset = seed * 10000L
  )
  
  # Shared initialization in [0,1]^2
  d <- 2
  n0 <- 10
  X_init <- maximinLHS(n0, d)
  y_init <- obj(X_init)
  
  # Run methods
  res_bass <- run_bass_bo(X_init, y_init, obj)
  res_gp   <- run_gp_bo(X_init, y_init, obj)
  res_rand <- run_random_search(X_init, y_init, obj)
  
  # Evaluate final test RMSE for each method using best params found
  eval_test_rmse <- function(X_eval, y_eval, method_name) {
    ib <- which.min(y_eval)
    best_u <- matrix(X_eval[ib, ], nrow = 1)
    pbest <- decode_enet_params(best_u, lmin = lambda_log10_min, lmax = lambda_log10_max)
    
    fit <- glmnet(
      x = x_train, y = y_train,
      alpha = pbest$alpha[1],
      lambda = pbest$lambda[1],
      standardize = FALSE
    )
    
    pred <- as.numeric(predict(fit, newx = x_test, s = pbest$lambda[1]))
    rmse <- sqrt(mean((pred - y_test)^2))
    
    tibble(
      seed = seed,
      method = method_name,
      alpha = pbest$alpha[1],
      lambda = pbest$lambda[1],
      log10_lambda = pbest$log10_lambda[1],
      cv_best = min(y_eval),
      test_rmse = rmse
    )
  }
  
  param_rows <- bind_rows(
    eval_test_rmse(res_bass$X_eval, res_bass$y_eval, "BASS-BO"),
    eval_test_rmse(res_gp$X_eval, res_gp$y_eval, "GP-BO"),
    eval_test_rmse(res_rand$X_eval, res_rand$y_eval, "Random Search")
  )
  
  curve_rows <- bind_rows(
    tibble(seed = seed, iter = 0:budget, method = "BASS-BO", best = res_bass$best),
    tibble(seed = seed, iter = 0:budget, method = "GP-BO", best = res_gp$best),
    tibble(seed = seed, iter = 0:budget, method = "Random Search", best = res_rand$best)
  )
  
  list(curves = curve_rows, params = param_rows)
}

# =========================
# Run experiment
# =========================
seeds <- seed_start + 0:(n_reps - 1)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Running Elastic Net BO comparison | reps=%d | budget=%d | n_cand=%d\n",
            n_reps, budget, n_cand))
cat(sprintf("Output dir: %s\n", out_dir))

res_list <- future_map(
  seeds,
  run_one_seed,
  .options = furrr_options(seed = TRUE)
)

all_runs <- bind_rows(map(res_list, "curves"))
all_params <- bind_rows(map(res_list, "params"))

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
  filter(iter == max(iter)) %>%
  ungroup() %>%
  group_by(method) %>%
  summarize(
    mean_final_cv = mean(best),
    sd_final_cv = sd(best),
    .groups = "drop"
  ) %>%
  arrange(mean_final_cv)

test_summary <- all_params %>%
  group_by(method) %>%
  summarize(
    mean_test_rmse = mean(test_rmse),
    sd_test_rmse = sd(test_rmse),
    mean_best_cv = mean(cv_best),
    sd_best_cv = sd(cv_best),
    .groups = "drop"
  ) %>%
  arrange(mean_test_rmse)

# Save outputs
write_csv(all_runs, file.path(out_dir, "all_runs.csv"))
write_csv(summary_curve, file.path(out_dir, "summary_curve.csv"))
write_csv(final_summary, file.path(out_dir, "final_summary_cv.csv"))
write_csv(all_params, file.path(out_dir, "best_params_and_test_rmse_by_seed.csv"))
write_csv(test_summary, file.path(out_dir, "test_rmse_summary.csv"))

# Plot convergence
p <- ggplot(summary_curve, aes(x = iter, y = mean_best, color = method, fill = method)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) +
  labs(
    title = sprintf("Elastic Net Tuning via BO (%d seeds)", n_reps),
    x = "Iteration (after initialization)",
    y = "Best CV RMSE so far (lower is better)"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "convergence_mean_ci.png"), plot = p, width = 8, height = 4, dpi = 150)

print(final_summary)
print(test_summary)
cat(sprintf("\nSaved files in: %s\n", normalizePath(out_dir)))