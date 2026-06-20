# =============================================================================
# candidates.R  --  Proposing and de-duplicating candidate points
# =============================================================================
# Each BO step scores a pool of candidate points in the unit cube [0,1]^d with
# the acquisition function and picks the best. We propose candidates with a
# parameter-free hybrid:
#
#   * half from a space-filling Latin Hypercube (global exploration), and
#   * half from a Gaussian cloud around the current best, whose width is the
#     incumbent's nearest-neighbour distance -- a length scale read straight off
#     the data that automatically shrinks as points cluster near the optimum.
#
# There are no local_frac / local_sd knobs to set: the split is a fixed 50/50
# and the width is derived, not tuned.
#
# Pure functions -- easy to unit-test.
# =============================================================================

#' Is point `x` already (almost) present in the rows of `X`?
#'
#' @param x   Numeric vector, the candidate point.
#' @param X   Matrix of evaluated points (one per row).
#' @param tol Distance below which two points count as identical.
#' @return    TRUE if `x` duplicates an existing row.
is_duplicate <- function(x, X, tol = 1e-10) {
  min(min_sqdist(matrix(as.numeric(x), nrow = 1), X)) <= tol^2
}

#' Squared distance from each candidate to its nearest evaluated point.
#'
#' Vectorised over candidates; loops over the (few) evaluated points. Used to
#' mask out candidates that duplicate something we have already measured.
#'
#' @param X_cand n x d matrix of candidates.
#' @param X_eval m x d matrix of evaluated points.
#' @return       Length-n vector of nearest squared distances.
min_sqdist <- function(X_cand, X_eval) {
  X_cand <- as.matrix(X_cand)
  X_eval <- as.matrix(X_eval)
  out <- rep(Inf, nrow(X_cand))
  for (i in seq_len(nrow(X_eval))) {
    diff <- sweep(X_cand, 2, X_eval[i, ], "-")
    out  <- pmin(out, rowSums(diff^2))
  }
  out
}

#' Plain space-filling candidates via a (fast) random Latin Hypercube.
#'
#' We use randomLHS rather than maximinLHS: it is far cheaper to draw a fresh set
#' every iteration and the coverage is plenty good for scoring candidates.
#'
#' @param n Number of candidates.
#' @param d Dimension.
#' @return  An n x d matrix in [0,1]^d.
space_filling_candidates <- function(n, d) {
  lhs::randomLHS(n, d)
}

#' A data-derived local search width: the incumbent's nearest-neighbour distance.
#'
#' Large early on (points are far apart, so we refine broadly), shrinking as the
#' design fills in around the optimum. Clipped to a sane range so the local
#' cloud is never degenerate or larger than the cube.
#'
#' @param X_eval   m x d matrix of evaluated points.
#' @param best_idx Row index of the current best point.
#' @return         A single positive standard deviation.
local_scale <- function(X_eval, best_idx) {
  X_eval <- as.matrix(X_eval)
  if (nrow(X_eval) < 2) return(0.1)
  others <- X_eval[-best_idx, , drop = FALSE]
  diff   <- sweep(others, 2, X_eval[best_idx, ], "-")
  dmin   <- sqrt(min(rowSums(diff^2)))
  min(max(dmin, 1e-2), 0.5)
}

#' Hybrid global + adaptive-local candidate set (parameter-free).
#'
#' @param X_eval m x d matrix of evaluated points.
#' @param y_eval Length-m objective values (best = smallest).
#' @param n_cand Total number of candidates to return.
#' @return       An n_cand x d matrix in [0,1]^d.
hybrid_candidates <- function(X_eval, y_eval, n_cand) {
  X_eval <- as.matrix(X_eval)
  d <- ncol(X_eval)

  n_local  <- floor(n_cand / 2)
  n_global <- n_cand - n_local

  # Global half: even coverage of the whole cube.
  X_global <- space_filling_candidates(n_global, d)

  # Local half: a Gaussian cloud around the incumbent, width set by the data,
  # clipped back into the unit cube.
  best_idx <- which.min(y_eval)
  x_best   <- X_eval[best_idx, ]
  s        <- local_scale(X_eval, best_idx)
  X_local  <- matrix(
    rnorm(n_local * d, mean = rep(x_best, each = n_local), sd = s),
    ncol = d
  )
  X_local <- pmin(pmax(X_local, 0), 1)

  rbind(X_global, X_local)
}
