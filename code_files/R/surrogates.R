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

#' Work around a BASS bug that crashes prediction on purely categorical models.
#'
#' For a categorical model the RJMCMC posterior routinely contains intercept-only
#' draws (nbasis == 0) -- especially early in the BO loop, before BASS has fit any
#' categorical basis. BASS's continuous basis-matrix builder guards that case
#' (`if (nbasis > 0)`), but its categorical counterpart, makeBasisMatrixCat(),
#' does not: it runs `for (m in 1:nbasis)`, and in R `1:0` is `c(1, 0)`, so it
#' reads `n.int.cat[i, 1]` -- NA for a zero-basis model -- and dies with
#' "missing value where TRUE/FALSE needed". Cat-Ackley triggers this reliably.
#'
#' We install a guarded copy of makeBasisMatrixCat into BASS's namespace. The only
#' change is wrapping the loop in `if (nbasis > 0)`, exactly as the continuous
#' builder does; zero-basis draws then correctly predict the intercept. The fix is
#' idempotent, runs once per R session (so it also fires on each furrr worker, via
#' the call in make_bass_acquire's closure), and becomes a no-op if a future BASS
#' ships the guard itself.
.ensure_bass_cat_predict_fix <- function() {
  if (isTRUE(getOption("bass_cat_predict_fixed"))) return(invisible())

  patched <- function(i, nbasis, vars, xx, n.int, sub) {
    n <- nrow(xx)
    tbasis.mat <- matrix(nrow = nbasis + 1, ncol = n)
    tbasis.mat[1, ] <- 1
    if (nbasis > 0) {
      for (m in 1:nbasis) {
        if (n.int[i, m] == 0) {
          tbasis.mat[m + 1, ] <- 1
        } else {
          use <- 1:n.int[i, m]
          tbasis.mat[m + 1, ] <- makeBasisCat(vars[i, m, use], sub[[i]][[m]], xx)
        }
      }
    }
    tbasis.mat
  }
  # Resolve BASS's unqualified internals (e.g. makeBasisCat) from its namespace.
  environment(patched) <- asNamespace("BASS")
  utils::assignInNamespace("makeBasisMatrixCat", patched, ns = "BASS")
  options(bass_cat_predict_fixed = TRUE)
  invisible()
}

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
#' @param cfg    Config list. Uses `acquisition` ("ei" or "thompson").
#' @param schema Optional input schema (see objective_utils.R). When it marks
#'   categorical coordinates, the fit and prediction frames hand BASS genuine
#'   factor columns so it uses its categorical (subset-of-levels) basis instead
#'   of treating the coordinate as an ordered number. NULL => all-continuous, the
#'   original behaviour.
#' @return An `acquire(X_eval, y_eval, X_cand)` function (captures `cfg`, `schema`).
make_bass_acquire <- function(cfg, schema = NULL) {
  function(X_eval, y_eval, X_cand) {
    # Guard BASS's categorical predict path (no-op once applied; see above). Done
    # here, inside the exported closure, so it also runs on each furrr worker.
    if (!is.null(schema)) .ensure_bass_cat_predict_fix()

    X_eval <- as.matrix(X_eval)
    n_cand <- nrow(as.matrix(X_cand))

    # .samples_by_cand() tells the two predict() orientations apart by the
    # candidate count; a square draws-by-candidates matrix would be ambiguous.
    if (n_cand == BASS_KEEP) {
      stop(sprintf(paste0("n_cand (%d) equals the number of stored BASS ",
                          "posterior draws; the predict() orientation check ",
                          "cannot disambiguate a square matrix. Pick a ",
                          "different --n_cand."), n_cand))
    }

    # Standardise y for BASS's numerics; guard a zero spread.
    y_mean <- mean(y_eval)
    y_sd   <- sd(y_eval)
    if (!is.finite(y_sd) || y_sd < 1e-12) y_sd <- 1
    y_std      <- (y_eval - y_mean) / y_sd
    y_best_std <- min(y_std)               # best so far, standardised

    # With a categorical schema, fit/predict on factor frames; otherwise keep the
    # plain numeric matrix / data frame the continuous benchmarks have always used.
    xx_fit  <- if (is.null(schema)) X_eval else to_model_frame(X_eval, schema)
    newdata <- if (is.null(schema)) as.data.frame(X_cand) else
                 to_model_frame(X_cand, schema)

    fit <- BASS::bass(
      xx = xx_fit, y = y_std,
      nmcmc = BASS_NMCMC, nburn = BASS_NBURN, thin = BASS_THIN,
      verbose = FALSE
    )

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
#' @return An `acquire(X_eval, y_eval, X_cand)` function (captures `cfg`).
make_gp_acquire <- function(cfg) {
  function(X_eval, y_eval, X_cand) {
    n_cand <- nrow(as.matrix(X_cand))
    fit <- tryCatch(GPfit::GP_fit(X_eval, y_eval), error = function(e) NULL)

    if (is.null(fit)) {
      # Say so: with a flat predictive, EI is constant and this iteration's
      # pick is effectively random. Frequent fallbacks mean the GP-BO curve is
      # partly a random-search curve, which the analysis should know about.
      message(sprintf("GP-BO: GP_fit failed on n=%d points; flat predictive fallback.",
                      nrow(as.matrix(X_eval))))
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
