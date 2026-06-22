# Tests for the TPE (Optuna) baseline. They are skipped entirely unless
# `reticulate` and an importable `optuna` are present, so they never block the
# pure-R suite. When Optuna is available (e.g. RETICULATE_PYTHON points at a
# Python with optuna installed), they exercise the real sampler.

test_that("run_tpe matches run_bo's contract on a continuous objective", {
  skip_if_not(tpe_available(), "reticulate/optuna not available")

  obj <- load_objective("branin", 2)
  set.seed(1001)
  X_init <- lhs::maximinLHS(2 * obj$d + 1, obj$d)
  y_init <- obj$fn(X_init)
  cfg    <- list(budget = 10)

  res <- run_tpe(obj, cfg, X_init, y_init, seed = 1001)

  expect_length(res$best, cfg$budget + 1)        # same shape as run_bo()$best
  expect_equal(res$best[1], min(y_init))         # starts from the initial design
  expect_true(all(diff(res$best) <= 1e-9))       # best-so-far never worsens
  expect_true(all(is.finite(res$best)))
  expect_lt(tail(res$best, 1), res$best[1])      # and improves on Branin
})

test_that("run_tpe handles a categorical/mixed objective and is reproducible", {
  skip_if_not(tpe_available(), "reticulate/optuna not available")

  obj <- load_objective("func2C", 4)             # 2 categorical + 2 continuous
  set.seed(2002)
  X_init <- lhs::maximinLHS(2 * obj$d + 1, obj$d)
  y_init <- obj$fn(X_init)
  cfg    <- list(budget = 8)

  a <- run_tpe(obj, cfg, X_init, y_init, seed = 7)$best
  b <- run_tpe(obj, cfg, X_init, y_init, seed = 7)$best
  expect_equal(a, b)                             # same seed -> identical curve
  expect_length(a, cfg$budget + 1)
  expect_true(all(diff(a) <= 1e-9))
  expect_true(all(is.finite(a)))
})

test_that("run_tpe_experiment yields one TPE curve per seed", {
  skip_if_not(tpe_available(), "reticulate/optuna not available")

  cfg <- default_config()
  cfg$objective <- "func2C"
  cfg$d         <- 4
  cfg$budget    <- 5
  cfg$reps      <- 2

  runs <- run_tpe_experiment(cfg)

  expect_setequal(unique(runs$method), "TPE")
  expect_equal(nrow(runs), cfg$reps * (cfg$budget + 1))
  expect_setequal(unique(runs$seed), cfg$seed_start + 0:(cfg$reps - 1))
  expect_true(all(is.finite(runs$best)))
})
