# =============================================================================
# surrogates.R  --  The interchangeable surrogate models
# =============================================================================
# A "surrogate" is a cheap model of the expensive objective. In our BO loop a
# surrogate is just a function with a fixed shape:
#
#     surrogate(X_eval, y_eval, X_cand) -> list(mu, sd)
#
# i.e. given the points evaluated so far, predict the posterior mean (`mu`) and
# standard deviation (`sd`) at each candidate. Because every surrogate has the
# same shape, `run_bo()` (in bo_loop.R) does not care which one it is using.
#
# This file builds the two surrogates we compare -- BASS and a Gaussian Process
# -- from a config list (see config.R). Random Search needs no model and is
# handled directly inside the BO loop.
# =============================================================================

#' Build a BASS (Bayesian Adaptive Spline Surfaces) surrogate.
#'
#' BASS is a Bayesian version of MARS: it fits piecewise-linear "hinge" basis
#' functions, which capture sharp transitions that a smooth GP tends to wash
#' out. We add a few numerical safeguards described inline.
#'
#' @param cfg Config list (see `default_config()`). Uses: `bass_sd_floor`,
#'   `bass_sd_inflate`, `bass_degree_early`, `bass_degree_late`,
#'   `bass_switch_after`.
#' @return A surrogate function `(X_eval, y_eval, X_cand) -> list(mu, sd)`.
make_bass_surrogate <- function(cfg) {
  function(X_eval, y_eval, X_cand) {
    X_eval <- as.matrix(X_eval)

    # Standardise y so BASS works on a well-scaled target. Guard against a zero
    # spread (e.g. all-equal y early on), which would divide by ~0.
    y_mean <- mean(y_eval)
    y_sd   <- sd(y_eval)
    if (!is.finite(y_sd) || y_sd < 1e-12) y_sd <- 1
    y_std <- (y_eval - y_mean) / y_sd

    # Increase model complexity as data accumulates: start with additive linear
    # splines (degree 1), switch to interaction splines (degree 2) once we have
    # enough points to support them.
    deg <- if (nrow(X_eval) < cfg$bass_switch_after) {
      cfg$bass_degree_early
    } else {
      cfg$bass_degree_late
    }
    fit <- BASS::bass(xx = X_eval, y = y_std, degree = deg, verbose = FALSE)

    # BASS returns one prediction per posterior sample. The returned matrix
    # orientation can vary, so transpose if needed to get samples-by-candidate.
    pred_mat <- as.matrix(predict(fit, newdata = as.data.frame(X_cand)))
    if (ncol(pred_mat) != nrow(X_cand)) pred_mat <- t(pred_mat)

    # Posterior mean and SD per candidate, mapped back to the original y scale.
    mu <- colMeans(pred_mat) * y_sd + y_mean
    sd <- apply(pred_mat, 2, sd) * y_sd

    # Inflate the SD a little and enforce a floor. Without this the surrogate can
    # become overconfident and the search collapses onto one point too early.
    sd <- pmax(sd * cfg$bass_sd_inflate, cfg$bass_sd_floor)

    list(mu = mu, sd = sd)
  }
}

#' Build a Gaussian Process surrogate (the standard BO baseline).
#'
#' @param cfg Config list. Uses: `eps` (jitter added to the predictive variance
#'   for numerical safety).
#' @return A surrogate function `(X_eval, y_eval, X_cand) -> list(mu, sd)`.
make_gp_surrogate <- function(cfg) {
  function(X_eval, y_eval, X_cand) {
    fit  <- GPfit::GP_fit(X_eval, y_eval)
    pred <- predict(fit, X_cand)
    mu   <- pred$Y_hat
    sd   <- sqrt(pmax(pred$MSE, 0) + cfg$eps)
    list(mu = mu, sd = sd)
  }
}
