# =============================================================================
# bo_loop.R  --  The single, generic Bayesian Optimization loop
# =============================================================================
# The same loop drives every method; only the candidate generator and the
# acquisition (the "acquire" closure) change. There are no acquisition knobs
# here -- Expected Improvement is parameter-free.
#
# A "method" is just:
#     list(
#       name       = "BASS-BO",
#       candidates = function(X_eval, y_eval) -> matrix,            # or NULL
#       acquire    = function(X_eval, y_eval, X_cand) -> numeric    # or NULL
#     )
# where `acquire` returns a score per candidate (higher = better). A NULL
# `acquire` means Random Search, which needs no model. Both closures capture the
# config when the method is built (see make_methods), so they take no cfg here.
# =============================================================================

#' Run one Bayesian Optimization method from a shared starting design.
#'
#' @param objective List with `fn` (maps an n x d matrix in [0,1]^d to n values
#'   to be minimised) and `d` (the dimension).
#' @param method    Method description (see top of file).
#' @param cfg       Config list (see `default_config()`).
#' @param X_init    Initial design points (matrix, one per row, in [0,1]^d).
#' @param y_init    Objective values at `X_init`.
#' @return          A list with:
#'                    * `best` : numeric vector of length `budget + 1`, best-so-far
#'                               from the initial design (index 1) onward;
#'                    * `X`    : every evaluated point (initial design + picks);
#'                    * `y`    : the objective value at each evaluated point.
run_bo <- function(objective, method, cfg, X_init, y_init) {
  f      <- objective$fn
  schema <- objective$schema   # NULL for purely continuous objectives
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d      <- ncol(X_eval)

  best_so_far    <- numeric(cfg$budget + 1)
  best_so_far[1] <- min(y_eval)

  for (t in 1:cfg$budget) {
    # Duplicates are judged on the canonical representation (categorical
    # coordinates snapped to bin centres, see canonicalize): two encodings that
    # decode to the same level combination are the same objective input, so
    # re-evaluating one is a wasted iteration on a deterministic objective.
    X_seen <- canonicalize(X_eval, schema)

    if (is.null(method$acquire)) {
      # ---- Random Search: a fresh, non-duplicate point ----
      # Attempts are capped: on a small categorical space that is nearly
      # exhausted, an un-evaluated combination may not turn up, and accepting
      # a duplicate then is better than spinning forever.
      for (attempt in 1:100) {
        x_next <- matrix(runif(d), nrow = 1)
        if (!is_duplicate(canonicalize(x_next, schema), X_seen, cfg$dup_tol)) break
      }
    } else {
      # ---- Model-based step: propose, score, take the best new candidate ----
      X_cand <- method$candidates(X_eval, y_eval)
      score  <- method$acquire(X_eval, y_eval, X_cand)
      # Rule out candidates that duplicate an already-evaluated point. (If the
      # whole pool is masked -- conceivable only when a tiny categorical space
      # is all but exhausted -- which.max still returns a point; a duplicate
      # then costs one iteration, it does not break the loop.)
      score[min_sqdist(canonicalize(X_cand, schema), X_seen) <= cfg$dup_tol^2] <- -Inf
      x_next <- X_cand[which.max(score), , drop = FALSE]
    }

    # Evaluate the (expensive) objective and record progress.
    y_next <- f(x_next)
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
  }

  list(best = best_so_far, X = X_eval, y = y_eval)
}

#' Build the three methods we compare, all from one config.
#'
#' BASS-BO and GP-BO share the same candidate generator and the same acquisition
#' (Expected Improvement), so the surrogate is the only thing that differs --
#' which is exactly the comparison the thesis is about.
#'
#' For categorical/mixed objectives the `schema` is forwarded to BASS-BO's
#' acquisition, so BASS fits on genuine factors. GP-BO's acquisition gets no
#' schema on purpose: a plain GP has no categorical kernel, so it sees the raw
#' [0,1] coordinate -- a continuous relaxation of the categories. That gap is the
#' whole point of the comparison. The shared candidate generator, however, *does*
#' get the schema for both methods, so both score a pool with sensible categorical
#' moves (see candidates.R); keeping the generator common is what preserves the
#' surrogate as the only difference between BASS-BO and GP-BO.
#'
#' @param cfg    Config list (see `default_config()`).
#' @param schema Optional input schema from the objective (NULL = all continuous).
#' @return Named list of methods: BASS-BO, GP-BO, Random.
make_methods <- function(cfg, schema = NULL) {
  shared_candidates <- function(X_eval, y_eval)
    hybrid_candidates(X_eval, y_eval, cfg$n_cand, schema)

  list(
    "BASS-BO" = list(
      name       = "BASS-BO",
      candidates = shared_candidates,
      acquire    = make_bass_acquire(cfg, schema)
    ),
    "GP-BO" = list(
      name       = "GP-BO",
      candidates = shared_candidates,
      acquire    = make_gp_acquire(cfg)
    ),
    "Random" = list(
      name       = "Random",
      candidates = NULL,   # no model: pure random sampling
      acquire    = NULL
    )
  )
}
