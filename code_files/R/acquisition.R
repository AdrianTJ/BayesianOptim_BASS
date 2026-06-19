# =============================================================================
# acquisition.R  --  Deciding which candidate to evaluate next
# =============================================================================
# Given the surrogate's prediction at each candidate (a posterior mean `mu` and
# standard deviation `sd`), the acquisition function turns those numbers into a
# ranking. We use Lower Confidence Bound (LCB) because we are minimising:
#
#     LCB(x) = mu(x) - kappa * sd(x)
#
# A larger `kappa` favours exploration (high-uncertainty points); a smaller one
# favours exploitation (low-mean points). We shrink `kappa` over the run so the
# search explores early and exploits late.
#
# Pure functions only -- easy to unit-test.
# =============================================================================

#' Exploration weight `kappa` for the current iteration.
#'
#' Linearly decays from `kappa_start` (iteration 1) to `kappa_end`
#' (iteration `budget`), so early steps explore and later steps exploit.
#'
#' @param t           Current iteration (1-based).
#' @param budget      Total number of iterations.
#' @param kappa_start Exploration weight at the first iteration.
#' @param kappa_end   Exploration weight at the last iteration.
#' @return            The interpolated `kappa` for iteration `t`.
kappa_decay <- function(t, budget, kappa_start, kappa_end) {
  kappa_start + (kappa_end - kappa_start) * (t - 1) / max(1, budget - 1)
}

#' Rank candidates from most to least promising.
#'
#' By default ranks by ascending LCB (best = lowest mean minus uncertainty).
#' When `explore = TRUE` it instead ranks by descending `sd`, i.e. a pure
#' exploration step that picks the most uncertain candidate. We use the explore
#' mode periodically to avoid getting stuck in a local basin.
#'
#' @param mu      Posterior mean at each candidate.
#' @param sd      Posterior standard deviation at each candidate.
#' @param kappa   Exploration weight (see `kappa_decay()`).
#' @param explore If TRUE, rank by uncertainty instead of LCB.
#' @return        Integer vector of candidate indices, best first.
acquisition_order <- function(mu, sd, kappa, explore = FALSE) {
  if (explore) {
    order(sd, decreasing = TRUE)
  } else {
    order(mu - kappa * sd)
  }
}
