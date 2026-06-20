# Tests for the objective helpers and the named-objective loader.

test_that("scale01_to_bounds maps the unit cube onto the physical box", {
  lower <- c(-5, 0)
  upper <- c(10, 15)
  corners <- matrix(c(0, 0,
                      1, 1), ncol = 2, byrow = TRUE)
  out <- scale01_to_bounds(corners, lower, upper)
  expect_equal(out[1, ], lower)   # (0,0) -> lower corner
  expect_equal(out[2, ], upper)   # (1,1) -> upper corner
})

test_that("vectorize_target rescales and evaluates row by row", {
  # Sum the physical coordinates; on [0,10]^1 the point 0.5 maps to 5.
  fn <- function(x) sum(x)
  vfn <- vectorize_target(fn, list(lower = 0, upper = 10))
  expect_equal(vfn(matrix(c(0, 0.5, 1), ncol = 1)), c(0, 5, 10))
})

test_that("branin reaches its known global minimum (~0.397887)", {
  # One of Branin's three global minimisers in physical space.
  val <- branin(c(pi, 2.275))
  expect_equal(val, 0.397887, tolerance = 1e-4)
})

test_that("load_objective wires up dimension and evaluation", {
  obj <- load_objective("synthetic", d = 3)
  expect_equal(obj$d, 3)
  y <- obj$fn(matrix(runif(5 * 3), ncol = 3))
  expect_length(y, 5)

  # Branin is 2-D only; asking for another dimension should error.
  expect_error(load_objective("branin", d = 3))
  # Unknown names should error clearly.
  expect_error(load_objective("not_a_function", d = 2))
})
