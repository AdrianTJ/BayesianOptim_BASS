# =============================================================================
# surrogates.R  --  Surrogate models, each exposed as an "acquire" function
# =============================================================================
# In the BO loop a method is defined by how it scores candidates. Each surrogate
# is therefore wrapped as a single closure:
#
#     acquire(X_eval, y_eval, X_cand, cfg) -> numeric (higher = more promising)
#
# The closure fits the surrogate on the current data and returns the acquisition
# value (Expected Improvement) at every candidate. Encapsulating "fit + predict +
# score" behind one function is what lets run_bo() stay model-agnostic.
# =============================================================================

# --- BASS MCMC settings -------------------------------------------------------
# Internal numerical settings (NOT user tuning knobs). They trade a little
# posterior resolution for speed inside the BO inner loop: ~4k RJMCMC iterations
# instead of BASS's default 10k, keeping 200 posterior samples
# ((NMCMC - NBURN) / THIN). This is the main BASS speed-up.
BASS_NMCMC <- 4000L
BASS_NBURN <- 2000L
BASS_THIN  <- 10L
BASS_KEEP  <- (BASS_NMCMC - BASS_NBURN) %/% BASS_THIN   # 200 stored draws

#' Orient a predict.bass() matrix to rows = samples, columns = candidates.
#'
#' predict.bass can return either orientation; we know the candidate count, so
#' transpose when the rows index candidates instead of samples.
#'
#' @param m      Matrix returned by predict.bass().
#' @param n_cand Number of candidates that were predicted.
#' @return       A samples x candidates matrix.
.samples_by_cand <- function(m, n_cand) {
  m <- as.matrix(m)
  if (nrow(m) == n_cand) t(m) else m
}

#' Build the BASS acquisition closure.
#'
#' Fits BASS to the standardised responses, then scores candidates with either
#' Monte Carlo Expected Improvement (default) or Thompson sampling. Both work on
#' the standardised scale: EI and the Thompson argmin are invariant to the
#' positive affine standardisation, so no back-transform is needed.
#'
#' @param cfg Config list. Uses `acquisition` ("ei" or "thompson").
#' @return An `acquire(X_eval, y_eval, X_cand, cfg)` function.
make_bass_acquire <- function(cfg) {
  function(X_eval, y_eval, X_cand, cfg) {
    X_eval <- as.matrix(X_eval)
    n_cand <- nrow(as.matrix(X_cand))

    # Standardise y for BASS's numerics; guard a zero spread.
    y_mean <- mean(y_eval)
    y_sd   <- sd(y_eval)
    if (!is.finite(y_sd) || y_sd < 1e-12) y_sd <- 1
    y_std      <- (y_eval - y_mean) / y_sd
    y_best_std <- min(y_std)               # best so far, standardised

    fit <- BASS::bass(
      xx = X_eval, y = y_std,
      nmcmc = BASS_NMCMC, nburn = BASS_NBURN, thin = BASS_THIN,
      verbose = FALSE
    )
    newdata <- as.data.frame(X_cand)

    if (identical(cfg$acquisition, "thompson")) {
      # Thompson sampling: draw ONE plausible response surface from the
      # posterior and head toward its minimum. Predicting a single draw is very
      # cheap. Maximising the negated draw == minimising the sampled surface.
      idx  <- sample.int(BASS_KEEP, 1)
      draw <- as.numeric(predict(fit, newdata, mcmc.use = idx))
      -draw
    } else {
      # Monte Carlo Expected Improvement over the full posterior draws.
      draws <- predict(fit, newdata, mcmc.use = seq_len(BASS_KEEP))
      draws <- .samples_by_cand(draws, n_cand)
      ei_mc(draws, y_best_std)
    }
  }
}

#' Build the Gaussian Process acquisition closure (the baseline).
#'
#' Fits a GP and scores candidates with closed-form Expected Improvement -- the
#' same acquisition principle as BASS, so only the surrogate differs. If the GP
#' fit fails (e.g. a near-singular correlation matrix on a noisy objective), we
#' fall back to a flat predictive (mean and spread of the data) so the run
#' continues instead of crashing.
#'
#' @param cfg Config list. Uses `eps` (jitter on the predictive variance).
#' @return An `acquire(X_eval, y_eval, X_cand, cfg)` function.
make_gp_acquire <- function(cfg) {
  function(X_eval, y_eval, X_cand, cfg) {
    n_cand <- nrow(as.matrix(X_cand))
    fit <- tryCatch(GPfit::GP_fit(X_eval, y_eval), error = function(e) NULL)

    if (is.null(fit)) {
      y_spread <- sd(y_eval)
      if (!is.finite(y_spread) || y_spread < 1e-12) y_spread <- 1e-6
      mu <- rep(mean(y_eval), n_cand)
      sd <- rep(y_spread,     n_cand)
    } else {
      pred <- predict(fit, X_cand)
      mu   <- pred$Y_hat
      sd   <- sqrt(pmax(pred$MSE, 0) + cfg$eps)
    }
    ei_gaussian(mu, sd, min(y_eval))
  }
}
