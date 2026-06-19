# Tests for the generic run_bo() loop.
#
# We test the loop mechanics with a FAKE surrogate (one that simply knows the
# true objective), so these tests are fast and do not need the BASS/GPfit
# packages. A guarded smoke test exercises the real surrogates if available.

# A simple convex objective on [0,1]^d with its minimum at the origin.
sphere_objective <- function(d) {
  list(name = "sphere", d = d, fn = function(X) rowSums(as.matrix(X)^2))
}

# Minimal config for a short run.
tiny_cfg <- function(budget = 15) {
  cfg <- default_config()
  cfg$budget <- budget
  cfg$n_cand <- 200
  cfg
}

test_that("best-so-far is non-increasing and of the right length", {
  set.seed(42)
  cfg <- tiny_cfg()
  obj <- sphere_objective(2)

  # Oracle method: the "surrogate" returns the true objective with zero
  # uncertainty, so the loop should greedily descend toward the optimum.
  oracle <- list(
    name        = "oracle",
    surrogate   = function(X_eval, y_eval, X_cand)
      list(mu = rowSums(X_cand^2), sd = rep(0, nrow(X_cand))),
    candidates  = function(X_eval, y_eval, cfg) lhs_candidates(cfg$n_cand, ncol(X_eval)),
    kappa_fn    = function(t, cfg) 0,
    use_explore = FALSE
  )

  X_init <- lhs_candidates(8, 2)
  y_init <- obj$fn(X_init)
  best <- run_bo(obj, oracle, cfg, X_init, y_init)

  expect_length(best, cfg$budget + 1)
  expect_true(all(diff(best) <= 0))          # never gets worse
  expect_lt(best[length(best)], best[1])     # actually improves
})

test_that("Random Search runs and never gets worse", {
  set.seed(7)
  cfg <- tiny_cfg()
  obj <- sphere_objective(2)
  rnd <- make_methods(cfg)[["Random"]]

  X_init <- lhs_candidates(8, 2)
  y_init <- obj$fn(X_init)
  best <- run_bo(obj, rnd, cfg, X_init, y_init)

  expect_length(best, cfg$budget + 1)
  expect_true(all(diff(best) <= 0))
})

test_that("real BASS and GP surrogates run end to end (if installed)", {
  skip_if_not_installed("BASS")
  skip_if_not_installed("GPfit")

  set.seed(1)
  cfg <- tiny_cfg(budget = 3)   # keep it short: model fitting is slow
  obj <- sphere_objective(2)
  methods <- make_methods(cfg)

  X_init <- lhs_candidates(8, 2)
  y_init <- obj$fn(X_init)

  for (nm in c("BASS-BO", "GP-BO")) {
    best <- run_bo(obj, methods[[nm]], cfg, X_init, y_init)
    expect_length(best, cfg$budget + 1)
    expect_true(all(diff(best) <= 0))
  }
})
