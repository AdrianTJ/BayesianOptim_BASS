# Tests for the acquisition function and the kappa decay schedule.

test_that("kappa_decay hits its endpoints and interpolates linearly", {
  # First iteration uses kappa_start, last uses kappa_end.
  expect_equal(kappa_decay(1,  budget = 10, kappa_start = 3.5, kappa_end = 1.5), 3.5)
  expect_equal(kappa_decay(10, budget = 10, kappa_start = 3.5, kappa_end = 1.5), 1.5)
  # Midpoint sits halfway between the two.
  mid <- kappa_decay(5.5, budget = 10, kappa_start = 3.5, kappa_end = 1.5)
  expect_equal(mid, 2.5)
})

test_that("acquisition_order ranks by LCB by default", {
  mu <- c(1.0, 0.0, 2.0)
  sd <- c(0.0, 0.0, 0.0)
  # With sd = 0, lowest mean wins: index 2, then 1, then 3.
  expect_equal(acquisition_order(mu, sd, kappa = 1), c(2, 1, 3))
})

test_that("acquisition_order in explore mode ranks by uncertainty", {
  mu <- c(0.0, 0.0, 0.0)
  sd <- c(0.1, 0.9, 0.5)
  # Explore mode ignores mu and picks the most uncertain first: 2, 3, 1.
  expect_equal(acquisition_order(mu, sd, kappa = 1, explore = TRUE), c(2, 3, 1))
})
