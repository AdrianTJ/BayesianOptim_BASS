# Tests for the generic run_bo() loop.
#
# Loop mechanics are tested with an "oracle" method (one that scores candidates
# by the true objective), so these tests are fast and need no BASS/GPfit. A
# guarded test exercises the real surrogates if the packages are installed.

# Convex sphere objective on [0,1]^d with its minimum at the origin.
sphere_objective <- function(d) {
  list(name = "sphere", d = d, fn = function(X) rowSums(as.matrix(X)^2))
}

tiny_cfg <- function(budget = 15, n_cand = 200) {
  cfg <- default_config()
  cfg$budget <- budget
  cfg$n_cand <- n_cand
  cfg
}

test_that("best-so-far is non-increasing and improves under an oracle", {
  set.seed(42)
  cfg <- tiny_cfg()
  obj <- sphere_objective(2)

  # Oracle: score = -(true objective), so the loop greedily descends.
  oracle <- list(
    name       = "oracle",
    candidates = function(X_eval, y_eval)
      space_filling_candidates(cfg$n_cand, ncol(as.matrix(X_eval))),
    acquire    = function(X_eval, y_eval, X_cand) -rowSums(X_cand^2)
  )

  X_init <- space_filling_candidates(8, 2)
  y_init <- obj$fn(X_init)
  best   <- run_bo(obj, oracle, cfg, X_init, y_init)$best

  expect_length(best, cfg$budget + 1)
  expect_true(all(diff(best) <= 0))        # never gets worse
  expect_lt(best[length(best)], best[1])   # actually improves
})

test_that("Random Search runs and never gets worse", {
  set.seed(7)
  cfg <- tiny_cfg()
  obj <- sphere_objective(2)
  rnd <- make_methods(cfg)[["Random"]]

  X_init <- space_filling_candidates(8, 2)
  y_init <- obj$fn(X_init)
  best   <- run_bo(obj, rnd, cfg, X_init, y_init)$best

  expect_length(best, cfg$budget + 1)
  expect_true(all(diff(best) <= 0))
})

test_that("no method re-evaluates a categorical combination (combo-level dedup)", {
  set.seed(11)
  ca  <- make_cat_ackley(3L, L = 5L)               # 125 combinations
  obj <- list(name = "cat_ackley_small", d = ca$d, fn = ca$fn, schema = ca$schema)

  cfg <- tiny_cfg(budget = 25, n_cand = 300)

  combo_of <- function(X) apply(canonicalize(X, obj$schema), 1, paste, collapse = "|")

  # Oracle-guided loop (EI would exercise BASS; the dedup path is the same).
  oracle <- list(
    name       = "oracle",
    candidates = function(X_eval, y_eval)
      hybrid_candidates(X_eval, y_eval, cfg$n_cand, obj$schema),
    acquire    = function(X_eval, y_eval, X_cand) -obj$fn(X_cand)
  )
  rnd <- make_methods(cfg, obj$schema)[["Random"]]

  X_init <- space_filling_candidates(8, obj$d)
  y_init <- obj$fn(X_init)

  for (m in list(oracle, rnd)) {
    res    <- run_bo(obj, m, cfg, X_init, y_init)
    combos <- combo_of(res$X)
    # 8 + 25 = 33 evaluations over 125 combinations: every one must be fresh
    # (the initial design may collide with itself; the loop's picks must not).
    picks  <- combos[(nrow(X_init) + 1):length(combos)]
    expect_false(any(picks %in% combos[1:nrow(X_init)]) || anyDuplicated(picks) > 0)
  }
})

test_that("real BASS (EI and Thompson) and GP run end to end (if installed)", {
  skip_if_not_installed("BASS")
  skip_if_not_installed("GPfit")

  obj    <- sphere_objective(2)
  X_init <- space_filling_candidates(8, 2)
  y_init <- obj$fn(X_init)

  for (acq in c("ei", "thompson")) {
    cfg <- tiny_cfg(budget = 3, n_cand = 100)
    cfg$acquisition <- acq
    methods <- make_methods(cfg)
    for (nm in c("BASS-BO", "GP-BO")) {
      best <- run_bo(obj, methods[[nm]], cfg, X_init, y_init)$best
      expect_length(best, cfg$budget + 1)
      expect_true(all(diff(best) <= 0))
    }
  }
})
