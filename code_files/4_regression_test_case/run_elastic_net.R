#!/usr/bin/env Rscript

# =============================================================================
# run_elastic_net.R  --  Real-world case study: tuning an Elastic Net with BO
# =============================================================================
# Compares BASS-BO, GP-BO and Random Search on a genuine ML task: choosing the
# Elastic Net (alpha, lambda) that minimises cross-validated RMSE on the Boston
# Housing data, then reports how the chosen model does on a held-out test set.
#
# The optimisation itself is delegated to the shared R/ library -- this script
# only supplies the objective (enet_objective.R) and the data plumbing. It uses
# the exact same parameter-free EI optimisers as the synthetic benchmarks.
#
# Example:
#   Rscript code_files/4_regression_test_case/run_elastic_net.R --reps=50 --budget=100
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lhs)
  library(BASS)
  library(GPfit)
  library(glmnet)
  library(MASS)      # Boston housing data
  library(future)
  library(furrr)
})

# --- Load the shared BO library + the Elastic Net objective -------------------
this_file  <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_dir <- if (length(this_file)) dirname(normalizePath(this_file)) else getwd()
lib_dir    <- normalizePath(file.path(script_dir, "..", "R"))

source(file.path(lib_dir, "bootstrap.R"))
source_library(lib_dir)
source(file.path(script_dir, "enet_objective.R"))

# --- Configuration ------------------------------------------------------------
# Start from the library defaults and add the case-specific settings, so every
# value (including --nfolds, --lambda_log10_min, ...) is settable on the CLI.
enet_default_config <- function() {
  cfg <- default_config()
  cfg$objective <- "elastic_net"
  cfg$d         <- 2
  cfg$budget    <- 100
  cfg$n_cand    <- 1500
  cfg$reps      <- 50
  cfg$out_dir   <- "results_enet_bo"
  cfg$train_frac        <- 0.8
  cfg$nfolds            <- 5L
  cfg$lambda_log10_min  <- -5
  cfg$lambda_log10_max  <- 1
  cfg$cache_digits      <- 6L
  cfg
}
cfg <- parse_cli_args(commandArgs(trailingOnly = TRUE), enet_default_config())

plan(multisession, workers = max(1L, parallel::detectCores() - 1L))

# --- One replicate ------------------------------------------------------------
# Splits the data, builds the CV-RMSE objective, runs all methods from a shared
# start, and evaluates each method's best hyperparameters on the test set.
run_enet_seed <- function(seed, cfg) {
  set.seed(seed)

  data("Boston", package = "MASS")
  df  <- as_tibble(Boston)
  idx <- sample(seq_len(nrow(df)), size = floor(cfg$train_frac * nrow(df)))

  x_train_raw <- as.matrix(dplyr::select(df[idx, ],  -medv)); y_train <- df$medv[idx]
  x_test_raw  <- as.matrix(dplyr::select(df[-idx, ], -medv)); y_test  <- df$medv[-idx]

  # Standardise features using training statistics only.
  ctr <- colMeans(x_train_raw)
  scl <- apply(x_train_raw, 2, sd); scl[scl == 0] <- 1
  x_train <- scale(x_train_raw, center = ctr, scale = scl)
  x_test  <- scale(x_test_raw,  center = ctr, scale = scl)

  # CV-RMSE objective on [0,1]^2 (cached so repeat points are not re-fit).
  obj_fn <- make_cached_objective(
    make_enet_objective(x_train, y_train, cfg$nfolds,
                        cfg$lambda_log10_min, cfg$lambda_log10_max,
                        seed_offset = seed * 10000L),
    digits = cfg$cache_digits
  )
  objective <- list(name = "elastic_net", d = 2, fn = obj_fn)

  # Shared initial design, then run every method on it.
  X_init  <- randomLHS(10, 2)
  y_init  <- obj_fn(X_init)
  methods <- make_methods(cfg)
  results <- lapply(methods, function(m) run_bo(objective, m, cfg, X_init, y_init))

  # Convergence curves.
  curves <- bind_rows(lapply(names(results), function(nm)
    tibble(seed = seed, iter = 0:cfg$budget, method = nm, best = results[[nm]]$best)))

  # Test-set performance of each method's best hyperparameters.
  params <- bind_rows(lapply(names(results), function(nm) {
    r      <- results[[nm]]
    best_u <- matrix(r$X[which.min(r$y), ], nrow = 1)
    p      <- decode_enet_params(best_u, cfg$lambda_log10_min, cfg$lambda_log10_max)
    fit    <- glmnet(x_train, y_train, alpha = p$alpha[1], lambda = p$lambda[1],
                     standardize = FALSE)
    pred   <- as.numeric(predict(fit, newx = x_test, s = p$lambda[1]))
    tibble(seed = seed, method = nm, alpha = p$alpha[1], lambda = p$lambda[1],
           log10_lambda = p$log10_lambda[1], cv_best = min(r$y),
           test_rmse = sqrt(mean((pred - y_test)^2)))
  }))

  list(curves = curves, params = params)
}

# --- Run all seeds in parallel ------------------------------------------------
seeds <- cfg$seed_start + 0:(cfg$reps - 1)
dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Elastic Net BO | reps=%d | budget=%d | folds=%d | acquisition=%s\n",
            cfg$reps, cfg$budget, cfg$nfolds, cfg$acquisition))

res_list <- future_map(
  seeds, ~ run_enet_seed(.x, cfg),
  .options = furrr_options(
    seed = TRUE,
    packages = c("BASS", "GPfit", "lhs", "glmnet", "MASS")
  )
)

all_runs   <- bind_rows(map(res_list, "curves"))
all_params <- bind_rows(map(res_list, "params"))

# --- Summarise ----------------------------------------------------------------
# Convergence summary reuses the library helper; the test-set leaderboard is
# specific to this case.
summary_curve <- summarise_curve(all_runs)
final_cv      <- summarise_final(all_runs)
test_summary  <- all_params |>
  dplyr::group_by(method) |>
  dplyr::summarise(
    mean_test_rmse = mean(test_rmse), sd_test_rmse = sd(test_rmse),
    mean_best_cv   = mean(cv_best),   sd_best_cv   = sd(cv_best),
    .groups = "drop"
  ) |>
  dplyr::arrange(mean_test_rmse)

# --- Persist ------------------------------------------------------------------
readr::write_csv(all_runs,      file.path(cfg$out_dir, "all_runs.csv"))
readr::write_csv(summary_curve, file.path(cfg$out_dir, "summary_curve.csv"))
readr::write_csv(final_cv,      file.path(cfg$out_dir, "final_summary_cv.csv"))
readr::write_csv(all_params,    file.path(cfg$out_dir, "best_params_and_test_rmse_by_seed.csv"))
readr::write_csv(test_summary,  file.path(cfg$out_dir, "test_rmse_summary.csv"))

p <- ggplot(summary_curve, aes(x = iter, y = mean_best, color = method, fill = method)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) +
  labs(title = sprintf("Elastic Net tuning via BO (%d seeds)", cfg$reps),
       x = "Iteration (after initialisation)",
       y = "Best CV RMSE so far (lower is better)") +
  theme_minimal()
ggsave(file.path(cfg$out_dir, "convergence_mean_ci.png"), plot = p,
       width = 8, height = 4, dpi = 150)

print(final_cv)
print(test_summary)
cat(sprintf("\nArtifacts saved in: %s\n", normalizePath(cfg$out_dir)))
