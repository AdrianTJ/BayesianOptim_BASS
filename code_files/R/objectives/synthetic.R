# =============================================================================
# synthetic.R  --  A deliberately awkward test surface
# =============================================================================
# This is the hand-built objective used in the early multi-dimensional
# experiments. It is interesting precisely because it is NOT smooth: it mixes a
# global bowl, oscillations, a sharp bump, a valley, and a jump discontinuity.
# That jump is the kind of feature BASS's piecewise-linear basis can capture but
# a smooth GP tends to over-smooth -- which is the whole point of the thesis.
#
# Defined directly on the unit cube [0, 1]^d, so it needs no domain rescaling.
# Works for any dimension d >= 1; terms that need a second coordinate switch off
# when d == 1.
# =============================================================================

#' Complex synthetic objective on [0, 1]^d (to be minimised).
#'
#' @param X An n x d matrix (or a single point) of inputs in [0, 1]^d.
#' @return  Numeric vector of length n.
synthetic <- function(X) {
  X <- as.matrix(X)
  if (is.null(nrow(X))) X <- matrix(X, nrow = 1)

  # Global bowl pulling toward 0.35 in every dimension.
  base  <- rowSums((X - 0.35)^2)
  # Decaying oscillation along the first axis.
  osc1  <- sin(6 * pi * X[, 1]) * exp(-2 * X[, 1])
  # Second-axis oscillation and a mild interaction term (only when d >= 2).
  osc2  <- if (ncol(X) >= 2) 0.35 * cos(5 * pi * X[, 2]^2) else 0
  inter <- if (ncol(X) >= 2) 0.5 * X[, 1] * X[, 2] else 0

  # A sharp Gaussian bump (penalty) and a Gaussian valley (reward).
  c1     <- rep(0.2,  ncol(X))
  c2     <- rep(0.75, ncol(X))
  bump   <-  1.2 * exp(-35 * rowSums((X - matrix(c1, nrow(X), ncol(X), byrow = TRUE))^2))
  valley <- -1.0 * exp(-28 * rowSums((X - matrix(c2, nrow(X), ncol(X), byrow = TRUE))^2))

  # Jump discontinuity along the first axis.
  jump <- ifelse(X[, 1] > 0.62, 0.4, 0)

  base + osc1 + osc2 + inter + bump + valley + jump
}
