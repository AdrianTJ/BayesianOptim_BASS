# =============================================================================
# bo_loop.R  --  The single, generic Bayesian Optimization loop
# =============================================================================
# This is the heart of the refactor. The same loop drives every method; only the
# surrogate and the candidate generator change. Previously this loop was copied
# and lightly tweaked in four different scripts -- now it lives here once.
#
# A "method" is a small description of how to optimise (see make_methods()):
#     list(
#       name       = "BASS-BO",
#       surrogate  = function(X_eval, y_eval, X_cand) -> list(mu, sd),  # or NULL
#       candidates = function(X_eval, y_eval, cfg)    -> matrix,
#       kappa_fn   = function(t, cfg)                  -> numeric,
#       use_explore = TRUE/FALSE   # whether to take periodic exploration steps
#     )
# A NULL surrogate means Random Search, which needs no model.
# =============================================================================

#' Run one Bayesian Optimization method from a shared starting design.
#'
#' Records the best (smallest) objective value seen after each iteration. The
#' three methods in an experiment share the same `X_init`/`y_init` so the
#' comparison is fair.
#'
#' @param objective List with `fn` (maps an n x d matrix in [0,1]^d to n values,
#'   to be minimised) and `d` (the dimension).
#' @param method    Method description (see top of file).
#' @param cfg       Config list (see `default_config()`).
#' @param X_init    Initial design points (matrix, one per row, in [0,1]^d).
#' @param y_init    Objective values at `X_init`.
#' @return          Numeric vector of length `budget + 1`: best-so-far at
#'                  iteration 0 (initial design) through `budget`.
run_bo <- function(objective, method, cfg, X_init, y_init) {
  f      <- objective$fn
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  d      <- ncol(X_eval)

  best_so_far    <- numeric(cfg$budget + 1)
  best_so_far[1] <- min(y_eval)

  for (t in 1:cfg$budget) {
    if (is.null(method$surrogate)) {
      # ---- Random Search: sample a fresh, non-duplicate point ----
      repeat {
        x_next <- matrix(runif(d), nrow = 1)
        if (!is_duplicate(x_next, X_eval, cfg$dup_tol)) break
      }
    } else {
      # ---- Model-based step (BASS-BO or GP-BO) ----
      # 1. Propose candidates, 2. score them with the surrogate, 3. rank them
      #    with the acquisition function, 4. take the best non-duplicate.
      X_cand <- method$candidates(X_eval, y_eval, cfg)
      post   <- method$surrogate(X_eval, y_eval, X_cand)

      kappa_t <- method$kappa_fn(t, cfg)
      explore <- isTRUE(method$use_explore) && (t %% cfg$explore_every == 0)
      ord     <- acquisition_order(post$mu, post$sd, kappa_t, explore)

      # Walk the ranking until we hit a point we have not evaluated before.
      # Fall back to the top-ranked point if everything is a duplicate.
      pick <- ord[!sapply(ord, function(i)
        is_duplicate(X_cand[i, ], X_eval, cfg$dup_tol))][1]
      if (is.na(pick)) pick <- ord[1]

      x_next <- X_cand[pick, , drop = FALSE]
    }

    # Evaluate the (expensive) objective and record progress.
    y_next <- f(x_next)
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
  }

  best_so_far
}

#' Build the three methods we compare, all from one config.
#'
#' Each method plugs a surrogate (or none) and a candidate generator into the
#' shared `run_bo()` loop. BASS uses a decaying kappa (explore early, exploit
#' late); GP keeps kappa fixed, matching the original baseline.
#'
#' @param cfg Config list (see `default_config()`).
#' @return Named list of method descriptions: BASS-BO, GP-BO, Random.
make_methods <- function(cfg) {
  list(
    "BASS-BO" = list(
      name        = "BASS-BO",
      surrogate   = make_bass_surrogate(cfg),
      candidates  = function(X_eval, y_eval, cfg)
        hybrid_candidates(X_eval, y_eval, cfg$n_cand,
                          cfg$bass_local_frac, cfg$bass_local_sd),
      kappa_fn    = function(t, cfg)
        kappa_decay(t, cfg$budget, cfg$bass_kappa_start, cfg$bass_kappa_end),
      use_explore = TRUE
    ),
    "GP-BO" = list(
      name        = "GP-BO",
      surrogate   = make_gp_surrogate(cfg),
      candidates  = function(X_eval, y_eval, cfg)
        lhs_candidates(cfg$n_cand, ncol(as.matrix(X_eval))),
      kappa_fn    = function(t, cfg) cfg$kappa,   # fixed exploration weight
      use_explore = FALSE
    ),
    "Random" = list(
      name        = "Random",
      surrogate   = NULL,        # no model: pure random sampling
      candidates  = NULL,
      use_explore = FALSE
    )
  )
}
