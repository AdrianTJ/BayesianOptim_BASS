# Tests for the candidate generation and duplicate-detection helpers.

test_that("is_duplicate flags points within tolerance and ignores far points", {
  X <- matrix(c(0.1, 0.1,
                0.9, 0.9), ncol = 2, byrow = TRUE)

  # Exactly on an existing row -> duplicate.
  expect_true(is_duplicate(c(0.1, 0.1), X))
  # Within a generous tolerance -> duplicate.
  expect_true(is_duplicate(c(0.1, 0.1) + 1e-12, X, tol = 1e-6))
  # Clearly elsewhere -> not a duplicate.
  expect_false(is_duplicate(c(0.5, 0.5), X))
})

test_that("lhs_candidates returns the right shape inside the unit cube", {
  set.seed(1)
  X <- lhs_candidates(50, 3)
  expect_equal(dim(X), c(50, 3))
  expect_true(all(X >= 0 & X <= 1))
})

test_that("hybrid_candidates returns n_cand points in the unit cube", {
  set.seed(1)
  X_eval <- matrix(runif(10 * 2), ncol = 2)
  y_eval <- runif(10)

  X <- hybrid_candidates(X_eval, y_eval, n_cand = 100,
                         local_frac = 0.35, local_sd = 0.08)
  expect_equal(nrow(X), 100)
  expect_equal(ncol(X), 2)
  expect_true(all(X >= 0 & X <= 1))   # local cloud must be clipped into [0,1]
})
