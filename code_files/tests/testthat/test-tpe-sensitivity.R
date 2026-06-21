# Tests for the TPE hyperparameter-sensitivity sweep (R/tpe.R: run_tpe()'s
# `sampler_opts` argument and run_tpe_sweep_experiment()). They are skipped
# entirely unless `reticulate` and an importable `optuna` are present, exactly
# like test-tpe.R.

test_that("run_tpe still matches run_bo's contract when sampler_opts is passed", {
  skip_if_not(tpe_available(), "reticulate/optuna not available")

  obj <- load_objective("branin", 2)
  set.seed(1001)
  X_init <- lhs::maximinLHS(2 * obj$d + 1, obj$d)
  y_init <- obj$fn(X_init)
  cfg    <- list(budget = 10)

  gamma_q <- function(q) function(n) as.integer(ceiling(q * n))
  res <- run_tpe(obj, cfg, X_init, y_init, seed = 1001,
                 sampler_opts = list(gamma = gamma_q(0.25)))

  expect_length(res$best, cfg$budget + 1)
  expect_equal(res$best[1], min(y_init))
  expect_true(all(diff(res$best) <= 1e-9))
  expect_true(all(is.finite(res$best)))
})

test_that("different gamma quantiles are actually wired through to Optuna", {
  skip_if_not(tpe_available(), "reticulate/optuna not available")

  obj <- load_objective("branin", 2)
  set.seed(3003)
  X_init <- lhs::maximinLHS(2 * obj$d + 1, obj$d)
  y_init <- obj$fn(X_init)
  cfg    <- list(budget = 15)

  gamma_q <- function(q) function(n) as.integer(ceiling(q * n))
  a <- run_tpe(obj, cfg, X_init, y_init, seed = 7,
               sampler_opts = list(gamma = gamma_q(0.10)))$best
  b <- run_tpe(obj, cfg, X_init, y_init, seed = 7,
               sampler_opts = list(gamma = gamma_q(0.75)))$best

  expect_length(a, cfg$budget + 1)
  expect_length(b, cfg$budget + 1)
  expect_true(all(is.finite(a)) && all(is.finite(b)))
  # Not asserting a < b or vice versa -- only that gamma is not silently
  # ignored, i.e. the two configs need not produce identical curves.
  expect_false(identical(a, b))
})

test_that("run_tpe_sweep_experiment stacks one labeled curve per (seed, config)", {
  skip_if_not(tpe_available(), "reticulate/optuna not available")

  cfg <- default_config()
  cfg$objective <- "func2C"
  cfg$d         <- 4
  cfg$budget    <- 5
  cfg$reps      <- 2

  gamma_q <- function(q) function(n) as.integer(ceiling(q * n))
  configs <- list(
    "TPE (gamma=0.10)" = list(sampler_opts = list(gamma = gamma_q(0.10))),
    "TPE (gamma=0.50)" = list(sampler_opts = list(gamma = gamma_q(0.50)))
  )

  runs <- run_tpe_sweep_experiment(cfg, configs)

  expect_setequal(unique(runs$method), names(configs))
  expect_equal(nrow(runs), cfg$reps * (cfg$budget + 1) * length(configs))
  expect_setequal(unique(runs$seed), cfg$seed_start + 0:(cfg$reps - 1))
  expect_true(all(is.finite(runs$best)))
})
