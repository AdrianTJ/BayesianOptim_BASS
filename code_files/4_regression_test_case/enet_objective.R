# =============================================================================
# enet_objective.R  --  Elastic Net CV-RMSE as a BO objective
# =============================================================================
# Turns "tune an Elastic Net" into the standard objective the BO library expects:
# a function from the unit square [0,1]^2 to a scalar to be minimised. The two
# inputs are decoded into the Elastic Net mixing parameter (alpha) and penalty
# (lambda, on a log scale), and the objective value is the k-fold cross-validated
# RMSE on the training data.
#
# These helpers are application-specific (they depend on glmnet), so they live
# with the regression case rather than in the model-agnostic R/ library.
# =============================================================================

#' Decode unit-square inputs into Elastic Net (alpha, lambda).
#'
#' alpha maps linearly to [0,1]; lambda maps to a log10 grid [lmin, lmax].
#'
#' @param Xu   n x 2 matrix (or a single point) in [0,1]^2.
#' @param lmin Minimum log10(lambda).
#' @param lmax Maximum log10(lambda).
#' @return A tibble with columns alpha, lambda, log10_lambda.
decode_enet_params <- function(Xu, lmin = -5, lmax = 1) {
  Xu <- as.matrix(Xu)
  if (is.null(nrow(Xu))) Xu <- matrix(Xu, nrow = 1)

  alpha        <- pmin(pmax(Xu[, 1], 0), 1)
  u_lambda     <- pmin(pmax(Xu[, 2], 0), 1)
  log10_lambda <- lmin + (lmax - lmin) * u_lambda

  tibble::tibble(alpha = alpha, lambda = 10^log10_lambda, log10_lambda = log10_lambda)
}

#' Reproducible k-fold assignment for the rows of the training data.
#'
#' @param n    Number of observations.
#' @param k    Number of folds.
#' @param seed RNG seed for the assignment.
#' @return Integer vector of length n with fold ids in 1..k.
make_folds <- function(n, k = 5, seed = 1L) {
  set.seed(seed)
  sample(rep(seq_len(k), length.out = n))
}

#' Build the Elastic Net CV-RMSE objective over [0,1]^2.
#'
#' Captures the training data and a fixed fold assignment, and returns a function
#' that evaluates the mean cross-validated RMSE for each row of its input.
#'
#' @param x_train     Scaled training design matrix.
#' @param y_train     Training response.
#' @param nfolds      Number of CV folds.
#' @param lmin,lmax   log10(lambda) range (see decode_enet_params()).
#' @param seed_offset Offsets the fold-assignment seed (per experiment seed).
#' @return A function `(Xu) -> numeric` mapping [0,1]^2 points to CV RMSE.
make_enet_objective <- function(x_train, y_train, nfolds, lmin, lmax, seed_offset = 0L) {
  force(x_train); force(y_train); force(nfolds); force(lmin); force(lmax)
  fold_id <- make_folds(nrow(x_train), k = nfolds, seed = 123 + seed_offset)

  function(Xu) {
    pars <- decode_enet_params(Xu, lmin = lmin, lmax = lmax)
    out  <- numeric(nrow(pars))

    for (i in seq_len(nrow(pars))) {
      fold_rmse <- numeric(nfolds)
      for (k in seq_len(nfolds)) {
        tr <- fold_id != k
        va <- fold_id == k
        fit <- glmnet::glmnet(
          x = x_train[tr, , drop = FALSE], y = y_train[tr],
          alpha = pars$alpha[i], lambda = pars$lambda[i], standardize = FALSE
        )
        pred <- as.numeric(predict(fit, newx = x_train[va, , drop = FALSE], s = pars$lambda[i]))
        fold_rmse[k] <- sqrt(mean((pred - y_train[va])^2))
      }
      out[i] <- mean(fold_rmse)
    }
    out
  }
}

#' Memoise an objective by rounded input, so repeated points are not re-fit.
#'
#' @param base_obj The objective to wrap.
#' @param digits   Rounding precision for the cache key.
#' @return A function with the same signature as `base_obj`.
make_cached_objective <- function(base_obj, digits = 6) {
  cache <- new.env(parent = emptyenv())
  function(Xu) {
    Xu <- as.matrix(Xu)
    if (is.null(nrow(Xu))) Xu <- matrix(Xu, nrow = 1)
    vapply(seq_len(nrow(Xu)), function(i) {
      key <- paste(round(Xu[i, ], digits), collapse = "_")
      if (!exists(key, envir = cache, inherits = FALSE)) {
        assign(key, base_obj(Xu[i, , drop = FALSE]), envir = cache)
      }
      get(key, envir = cache, inherits = FALSE)
    }, numeric(1))
  }
}
