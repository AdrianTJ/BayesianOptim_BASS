# =============================================================================
# objective_utils.R  --  Shared helpers for benchmark objectives
# =============================================================================
# Benchmark functions are written on their natural physical domain (e.g. Branin
# on [-5,10] x [0,15]), but the BO loop always works on the unit cube [0, 1]^d.
# These helpers bridge that gap: scale a point from the cube to the physical box,
# wrap a scalar-input benchmark so it accepts a matrix of points, and -- for the
# categorical benchmarks -- decode unit-cube coordinates into category levels and
# assemble the factor data frame that BASS needs to use its categorical basis.
#
# Mixed/categorical inputs are carried through the loop as ordinary [0, 1]^d
# coordinates (so candidate generation, de-duplication and the GP baseline are
# untouched). A `schema` describes which coordinates are categorical:
#
#     schema = list(
#       types  = c("cat", "cat", "cont", "cont"),  # one per dimension
#       levels = c(3L, 5L, NA, NA)                  # #levels for "cat", NA else
#     )
#
# An objective decodes the categorical coordinates itself; the BASS surrogate
# uses the same schema to hand BASS genuine factor columns (see to_model_frame).
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
  # Scale first, then shift. (The previous version added X an extra time, which
  # distorted every benchmark's domain -- correct at the lower corner but off
  # by X*1 elsewhere.)
  sweep(sweep(X, 2, upper - lower, `*`), 2, lower, `+`)
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

#' Decode a unit-interval coordinate into a 1..L category level.
#'
#' Splits [0, 1] into L equal bins; `u` in bin k maps to level k. The right
#' endpoint (u == 1) maps to level L rather than spilling over to L + 1.
#'
#' @param u Numeric vector in [0, 1].
#' @param L Number of category levels (integer >= 1).
#' @return  Integer vector of levels in 1..L.
decode_levels <- function(u, L) {
  pmin(as.integer(floor(u * L)) + 1L, L)
}

#' Assemble the model frame BASS sees, turning categorical columns into factors.
#'
#' Continuous columns are kept as their raw [0, 1] coordinate; categorical
#' columns are decoded to 1..L and wrapped as factors with a fixed, complete set
#' of levels (so the fit and the prediction frames always agree, even if some
#' level is absent from one of them). This is what lets BASS engage its
#' categorical basis instead of treating the column as an ordered number.
#'
#' @param X      n x d matrix of points in [0, 1]^d.
#' @param schema List with `types` ("cat"/"cont") and `levels` (see file header).
#' @return       A data frame with d columns; categorical ones are factors.
to_model_frame <- function(X, schema) {
  X  <- as.matrix(X)
  if (is.null(nrow(X))) X <- matrix(X, nrow = 1)
  df <- as.data.frame(X)
  names(df) <- paste0("V", seq_len(ncol(X)))

  for (j in which(schema$types == "cat")) {
    L <- schema$levels[j]
    df[[j]] <- factor(decode_levels(X[, j], L), levels = seq_len(L))
  }
  df
}
