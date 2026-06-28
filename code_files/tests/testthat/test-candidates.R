# Tests for candidate generation and duplicate detection.

test_that("min_sqdist returns the nearest squared distance per candidate", {
  X_eval <- matrix(c(0, 0,
                     1, 1), ncol = 2, byrow = TRUE)
  X_cand <- matrix(c(0.0, 0.0,
                     0.5, 0.5), ncol = 2, byrow = TRUE)
  d2 <- min_sqdist(X_cand, X_eval)
  expect_equal(d2[1], 0)      # lands exactly on (0,0)
  expect_equal(d2[2], 0.5)    # equidistant from both corners: 0.25 + 0.25
})

test_that("is_duplicate flags near points and ignores far ones", {
  X <- matrix(c(0.1, 0.1,
                0.9, 0.9), ncol = 2, byrow = TRUE)
  expect_true(is_duplicate(c(0.1, 0.1), X))
  expect_true(is_duplicate(c(0.1, 0.1) + 1e-12, X, tol = 1e-6))
  expect_false(is_duplicate(c(0.5, 0.5), X))
})

test_that("space_filling_candidates returns the right shape in the unit cube", {
  set.seed(1)
  X <- space_filling_candidates(50, 3)
  expect_equal(dim(X), c(50, 3))
  expect_true(all(X >= 0 & X <= 1))
})

test_that("hybrid_candidates returns exactly n_cand points in the unit cube", {
  set.seed(1)
  X_eval <- matrix(runif(10 * 2), ncol = 2)
  y_eval <- runif(10)
  X <- hybrid_candidates(X_eval, y_eval, n_cand = 101)  # odd -> tests the split
  expect_equal(nrow(X), 101)
  expect_equal(ncol(X), 2)
  expect_true(all(X >= 0 & X <= 1))   # local cloud must be clipped into [0,1]
})

test_that("schema-aware local half makes Hamming-local categorical moves", {
  set.seed(1)
  d <- 5L; L <- 7L
  schema <- list(types = rep("cat", d), levels = rep(L, d))
  X_eval <- matrix(runif(20 * d), ncol = d)
  y_eval <- runif(20)
  n_cand <- 400
  X <- hybrid_candidates(X_eval, y_eval, n_cand = n_cand, schema = schema)
  expect_equal(dim(X), c(n_cand, d))

  inc <- decode_levels(X_eval[which.min(y_eval), ], L)
  n_local <- floor(n_cand / 2)
  local_rows <- X[(n_cand - n_local + 1):n_cand, , drop = FALSE]
  levs <- t(apply(local_rows, 1, decode_levels, L = L))

  # Categorical coords sit on bin centres, so they decode back exactly.
  expect_true(all(abs(local_rows * L - (floor(local_rows * L) + 0.5)) < 1e-9))

  # Every local move differs from the incumbent in 1..3 coordinates (Hamming-local).
  ham <- rowSums(sweep(levs, 2, inc, "!=") != 0)
  expect_true(all(ham >= 1 & ham <= 3))

  # Flips can reach non-adjacent levels (not just index neighbours): some coord
  # should land more than one level away from the incumbent at least once.
  expect_true(any(abs(sweep(levs, 2, inc, "-")) > 1))
})

test_that("hybrid_candidates leaves continuous coords Gaussian under a mixed schema", {
  set.seed(2)
  # Two categorical + two continuous, like Func-2C.
  schema <- list(types = c("cat", "cat", "cont", "cont"),
                 levels = c(3L, 5L, NA, NA))
  X_eval <- matrix(runif(12 * 4), ncol = 4)
  y_eval <- runif(12)
  X <- hybrid_candidates(X_eval, y_eval, n_cand = 200, schema = schema)
  expect_equal(dim(X), c(200, 4))
  expect_true(all(X >= 0 & X <= 1))

  # Continuous coords are NOT snapped to bin centres (they stay genuinely spread).
  local_rows <- X[101:200, 3:4, drop = FALSE]
  expect_gt(length(unique(round(local_rows[, 1], 6))), 50)
})
