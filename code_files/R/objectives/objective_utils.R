# =============================================================================
# objective_utils.R  --  Shared helpers for benchmark objectives
# =============================================================================
# Benchmark functions are written on their natural physical domain (e.g. Branin
# on [-5,10] x [0,15]), but the BO loop always works on the unit cube [0, 1]^d.
# These two helpers bridge that gap: scale a point from the cube to the physical
# box, and wrap a scalar-input benchmark so it accepts a matrix of points.
# =============================================================================

#' Map points from [0, 1]^d to a physical box [lower, upper].
#'
#' @param X     n x d matrix of points in [0, 1]^d.
#' @param lower Numeric vector (length d) of lower bounds.
#' @param upper Numeric vector (length d) of upper bounds.
#' @return      n x d matrix of rescaled points.
scale01_to_bounds <- function(X, lower, upper) {
  X <- as.matrix(X)
  # column i  ->  lower[i] + X[, i] * (upper[i] - lower[i])
  sweep(X, 2, lower, `+`) +
    sweep(X, 2, upper - lower, `*`)
}

#' Wrap a scalar-input benchmark into a matrix-input, unit-cube objective.
#'
#' The returned function takes points in [0, 1]^d, rescales them to the
#' benchmark's physical domain, and evaluates the benchmark row by row.
#'
#' @param fn     A function of one numeric vector (a single point).
#' @param bounds List with `lower` and `upper` numeric vectors (length d).
#' @return       A function `(X) -> numeric vector`, X an n x d matrix in [0,1]^d.
vectorize_target <- function(fn, bounds) {
  force(fn)
  force(bounds)

  function(X) {
    X <- as.matrix(X)
    if (is.null(nrow(X))) X <- matrix(X, nrow = 1)

    X_phys <- scale01_to_bounds(X, bounds$lower, bounds$upper)
    apply(X_phys, 1, fn)
  }
}
