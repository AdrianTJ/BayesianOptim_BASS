# =============================================================================
# acquisition.R  --  Expected Improvement (the one acquisition we use)
# =============================================================================
# We minimise, so "improvement" at a point x over the best value seen so far
# (y_best) is  max(0, y_best - f(x)).  Expected Improvement (EI) is the average
# improvement under the surrogate's predictive distribution. EI is the classic,
# parameter-free BO acquisition -- there is no exploration weight to tune.
#
# Two flavours, one per surrogate:
#   * ei_gaussian() -- closed form, for the GP (Gaussian predictive).
#   * ei_mc()       -- Monte Carlo from posterior draws, for BASS (whose
#                      predictive is a non-Gaussian mixture of spline models).
#
# Using EI for BOTH surrogates is deliberate: it makes the surrogate the only
# difference between the methods, which is exactly what the thesis compares.
#
# Pure functions -- no model fitting, easy to unit-test.
# =============================================================================

#' Closed-form Expected Improvement for a Gaussian predictive (minimisation).
#'
#' EI(x) = (y_best - mu) * Phi(z) + sd * phi(z),  with z = (y_best - mu) / sd.
#' Where sd == 0 the predictive is a point mass, so EI is just the (clamped)
#' improvement.
#'
#' @param mu     Posterior mean at each candidate.
#' @param sd     Posterior standard deviation at each candidate (>= 0).
#' @param y_best Best (smallest) objective value observed so far.
#' @return       Non-negative EI value at each candidate (higher = better).
ei_gaussian <- function(mu, sd, y_best) {
  sd  <- pmax(sd, 0)
  imp <- y_best - mu                       # improvement if we hit the mean
  pos <- sd > 0
  z   <- ifelse(pos, imp / sd, 0)
  ei  <- imp * pnorm(z) + sd * dnorm(z)
  ei[!pos] <- pmax(imp[!pos], 0)           # degenerate (sd == 0) candidates
  pmax(ei, 0)
}

#' Monte Carlo Expected Improvement from posterior predictive draws.
#'
#' Given S posterior draws of the surrogate evaluated at each candidate, EI is
#' the average improvement across draws. This needs no Gaussian assumption, so
#' it uses BASS's full (skewed, multi-modal) predictive as-is.
#'
#' @param draws  S x n matrix of predictive draws (rows = posterior samples,
#'               columns = candidates).
#' @param y_best Best (smallest) objective value observed so far.
#' @return       Non-negative EI value at each candidate (length n).
ei_mc <- function(draws, y_best) {
  colMeans(pmax(y_best - draws, 0))
}
