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
