# =============================================================================
# candidates.R  --  Generating and de-duplicating candidate points
# =============================================================================
# Every Bayesian Optimization (BO) step needs a pool of candidate points in the
# unit cube [0, 1]^d to score with the acquisition function. This file holds the
# two candidate generators we use plus the duplicate check that stops us from
# evaluating (almost) the same point twice.
#
# Pure functions only -- no model fitting, no global state. That makes them easy
# to unit-test (see tests/testthat/test-candidates.R).
# =============================================================================

#' Is point `x` already (almost) present in the rows of `X`?
#'
#' Returns TRUE when `x` lies within Euclidean distance `tol` of any existing
#' row. Used to avoid re-evaluating the objective at a point we already know.
#'
#' @param x   Numeric vector, the candidate point.
#' @param X   Matrix of already-evaluated points (one point per row).
#' @param tol Distance below which two points count as the same.
#' @return    TRUE if `x` duplicates an existing row, otherwise FALSE.
is_duplicate <- function(x, X, tol = 1e-10) {
  x <- as.numeric(x)
  X <- as.matrix(X)
  # Squared distance from x to every row, compared against tol^2 (cheaper than
  # taking square roots).
  xmat <- matrix(x, nrow = nrow(X), ncol = ncol(X), byrow = TRUE)
  any(rowSums((X - xmat)^2) <= tol^2)
}

#' Plain space-filling candidates (used by the GP surrogate).
#'
#' A maximin Latin Hypercube spreads `n` points evenly across [0, 1]^d.
#'
#' @param n Number of candidates.
#' @param d Dimension.
#' @return  An `n` x `d` matrix of candidate points in [0, 1]^d.
lhs_candidates <- function(n, d) {
  lhs::maximinLHS(n, d)
}

#' Hybrid global + local candidates (used by the BASS surrogate).
#'
#' Mixes two sources so the optimizer both explores widely and refines the
#' current best:
#'   - a global maximin Latin Hypercube covering the whole cube, and
#'   - a local Gaussian cloud sampled around the best point found so far.
#' The local points are clipped back into [0, 1]^d.
#'
#' @param X_eval     Matrix of evaluated points (one per row).
#' @param y_eval     Numeric vector of objective values (we minimise, so the
#'                   best point is the row with the smallest value).
#' @param n_cand     Total number of candidates to return.
#' @param local_frac Fraction of `n_cand` drawn from the local cloud (0..1).
#' @param local_sd   Standard deviation of the local Gaussian cloud.
#' @return           An `n_cand` x `d` matrix of candidate points in [0, 1]^d.
hybrid_candidates <- function(X_eval, y_eval, n_cand, local_frac = 0.35,
                              local_sd = 0.08) {
  X_eval <- as.matrix(X_eval)
  d <- ncol(X_eval)

  # Split the budget between local refinement and global exploration.
  n_local  <- max(1L, round(n_cand * local_frac))
  n_global <- n_cand - n_local

  # Global half: spread evenly across the whole cube.
  X_global <- lhs::maximinLHS(n_global, d)

  # Local half: a Gaussian cloud centred on the current best point, then clipped
  # to stay inside the unit cube.
  x_best  <- X_eval[which.min(y_eval), ]
  X_local <- matrix(
    rnorm(n_local * d, mean = rep(x_best, each = n_local), sd = local_sd),
    ncol = d, byrow = FALSE
  )
  X_local <- pmin(pmax(X_local, 0), 1)

  rbind(X_global, X_local)
}
