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
# Categorical coordinates need a different notion of "local". A Gaussian nudge of
# the unit-cube coordinate is an *ordinal* move -- it can only reach the
# incumbent's level and its index-neighbours -- which is meaningless for an
# unordered factor (and actively wrong when the levels are permuted, as in
# Cat-Ackley). So when an objective supplies a `schema`, the local half instead
# makes Hamming-local moves on the categorical coordinates: each coordinate keeps
# the incumbent's level or flips to a uniformly-random *other* level with
# probability 1/n_cat (a derived rate, not a knob). Crucially, on MIXED problems
# a local row may keep the ENTIRE incumbent combination and only nudge the
# continuous coordinates -- that pure continuous refinement at the best-known
# combination is the main local-exploitation move BO has over Random Search, and
# an always-flip rule forbids it (which measurably capped func2C/func3C; see
# 3_categorical_diagnostics/). On purely categorical problems a zero-flip row
# would just duplicate the incumbent, so there at least one flip is forced.
# Continuous coordinates still get the Gaussian cloud. This generator is shared
# by BASS-BO and GP-BO, so the surrogate remains the only thing that differs.
#
# Pure functions -- easy to unit-test.
# =============================================================================

#' Canonical representation of points for duplicate detection.
#'
#' Two points whose categorical coordinates decode to the same levels are the
#' SAME input to the objective, whatever their raw [0,1] encodings. Snapping
#' each categorical coordinate to its bin centre `(level - 0.5) / L` makes such
#' points exactly equal, so the ordinary Euclidean duplicate checks below work
#' at the decoded-combination level: on a purely categorical problem a
#' "duplicate" is a revisited combination, and on a mixed problem it is the
#' same combination with (near-)identical continuous coordinates. Without this,
#' the loop happily re-evaluates combinations it has already measured -- pure
#' waste on a deterministic objective (instrumented runs showed ~2/3 of the
#' budget lost to revisits on a small categorical benchmark).
#'
#' @param X      n x d matrix of points in [0,1]^d.
#' @param schema Optional input schema (see objective_utils.R); NULL = identity.
#' @return       n x d matrix with categorical columns snapped to bin centres.
canonicalize <- function(X, schema = NULL) {
  X <- as.matrix(X)
  if (is.null(schema)) return(X)
  for (j in which(schema$types == "cat")) {
    L <- schema$levels[j]
    X[, j] <- (decode_levels(X[, j], L) - 0.5) / L
  }
  X
}

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

#' Replace the categorical coordinates of a local cloud with Hamming-local moves.
#'
#' Each row keeps the incumbent's level on most categorical coordinates: every
#' coordinate flips to a uniformly-random *other* level independently with
#' probability 1/#cat (so one flip in expectation) -- the correct notion of
#' "nearby" for an unordered factor. On mixed schemas a row may flip NOTHING,
#' keeping the incumbent's full combination so its continuous coordinates get a
#' pure local refinement; on purely categorical schemas a zero-flip row would
#' only duplicate the incumbent, so one flip is forced there. Chosen levels are
#' written back as bin centres, `(level - 0.5) / L`, which decode exactly to that
#' level (see decode_levels) and dedupe cleanly across iterations. Continuous
#' coordinates in `X_local` are left untouched.
#'
#' @param X_local n x d local-cloud matrix (continuous coords already filled in).
#' @param x_best  The incumbent point (length-d vector in [0,1]^d).
#' @param schema  Input schema with `types` and `levels` (see objective_utils.R).
#' @param cat_idx Integer indices of the categorical coordinates.
#' @return        `X_local` with its categorical columns rewritten.
local_categorical_moves <- function(X_local, x_best, schema, cat_idx) {
  n_cat    <- length(cat_idx)
  pure_cat <- all(schema$types == "cat")
  inc_lev  <- vapply(cat_idx,
                     function(j) as.integer(decode_levels(x_best[j], schema$levels[j])),
                     integer(1))

  for (r in seq_len(nrow(X_local))) {
    flip <- which(runif(n_cat) < 1 / n_cat)  # each coord flips w.p. 1/n_cat
    if (pure_cat && length(flip) == 0L) {
      flip <- sample.int(n_cat, 1L)          # zero flips = duplicate incumbent
    }
    for (c in seq_len(n_cat)) {
      j   <- cat_idx[c]
      L   <- schema$levels[j]
      lev <- inc_lev[c]
      if (c %in% flip) lev <- sample(setdiff(seq_len(L), lev), 1L)
      X_local[r, j] <- (lev - 0.5) / L       # bin centre -> decodes back to `lev`
    }
  }
  X_local
}

#' Hybrid global + adaptive-local candidate set (parameter-free).
#'
#' @param X_eval m x d matrix of evaluated points.
#' @param y_eval Length-m objective values (best = smallest).
#' @param n_cand Total number of candidates to return.
#' @param schema Optional input schema (see objective_utils.R). When it marks
#'   categorical coordinates, the local half makes Hamming-local moves on them
#'   instead of an (ill-defined) ordinal Gaussian nudge; NULL => all-continuous,
#'   the original behaviour.
#' @return       An n_cand x d matrix in [0,1]^d.
hybrid_candidates <- function(X_eval, y_eval, n_cand, schema = NULL) {
  X_eval <- as.matrix(X_eval)
  d <- ncol(X_eval)

  n_local  <- floor(n_cand / 2)
  n_global <- n_cand - n_local

  # Global half: even coverage of the whole cube (uniform levels for categoricals).
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

  # On categorical coordinates the Gaussian nudge is the wrong move; replace it
  # with Hamming-local flips around the incumbent's levels.
  if (!is.null(schema) && n_local > 0) {
    cat_idx <- which(schema$types == "cat")
    if (length(cat_idx)) {
      X_local <- local_categorical_moves(X_local, x_best, schema, cat_idx)
    }
  }

  rbind(X_global, X_local)
}
