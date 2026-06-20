# Tests for the Expected Improvement acquisition functions.

test_that("ei_gaussian is non-negative and correct at sd = 0", {
  # With no uncertainty, EI is just the clamped improvement max(0, y_best - mu).
  ei <- ei_gaussian(mu = c(0, 1, 2), sd = c(0, 0, 0), y_best = 1)
  expect_equal(ei, c(1, 0, 0))
})

test_that("ei_gaussian prefers a lower mean and rewards uncertainty", {
  # Lower predicted mean -> higher EI at the same uncertainty.
  e1 <- ei_gaussian(mu = c(0.0, 0.5), sd = c(1, 1), y_best = 1)
  expect_gt(e1[1], e1[2])

  # When the mean sits exactly at y_best (zero expected improvement from the
  # mean), more uncertainty still yields more EI.
  e2 <- ei_gaussian(mu = c(1, 1), sd = c(0.5, 1.0), y_best = 1)
  expect_gt(e2[2], e2[1])

  expect_true(all(e1 >= 0) && all(e2 >= 0))
})

test_that("ei_mc averages improvement across posterior draws", {
  # 3 samples x 2 candidates: candidate 1 always predicts 0, candidate 2 predicts 2.
  draws <- matrix(c(0, 2,
                    0, 2,
                    0, 2), nrow = 3, byrow = TRUE)
  # y_best = 1: candidate 1 improves by 1 every draw; candidate 2 never improves.
  expect_equal(ei_mc(draws, y_best = 1), c(1, 0))
})
