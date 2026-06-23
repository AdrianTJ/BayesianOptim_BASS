# =============================================================================
# tpe.R  --  Tree-structured Parzen Estimator baseline (via Optuna)
# =============================================================================
# The Tree-structured Parzen Estimator (TPE) is the de-facto standard surrogate
# for hyper-parameter optimization with mixed and categorical search spaces. Like
# BASS, and unlike a plain Gaussian process, it handles categorical inputs
# natively: it models the density of "good" and "bad" trials separately and, for
# a categorical variable, simply estimates a distribution over its levels. That
# makes it the most relevant external baseline for the categorical benchmarks.
#
# There is no mature native-R TPE implementation, so we use the reference one:
# Optuna's TPESampler, driven from R through the `reticulate` bridge. The loop
# below is wrapped so that TPE is run on exactly the same footing as the other
# methods -- it is seeded with the SAME initial design (injected as completed
# trials), given the SAME evaluation budget, and reports the SAME best-so-far
# curve -- so its result drops straight into the comparison alongside BASS-BO,
# GP-BO and Random Search.
#
# This file adds an optional dependency only: if `reticulate` or `optuna` is not
# available, tpe_available() returns FALSE and the experiment simply omits TPE,
# leaving the pure-R runs untouched.
#
# References: Bergstra, Bardenet, Bengio & Kegl (2011), "Algorithms for
# Hyper-Parameter Optimization" (the original TPE); Watanabe (2023),
# "Tree-structured Parzen Estimator: Understanding Its Algorithm Components"
# (arXiv:2304.11127); Akiba et al. (2019), "Optuna" (the implementation used).
# =============================================================================

#' Is the Optuna-backed TPE baseline runnable in this session?
#'
#' True only if `reticulate` is installed and it can see an importable `optuna`
#' module in the configured Python. Everything else degrades gracefully to a run
#' without TPE.
#'
#' @return TRUE if TPE can be run, FALSE otherwise.
tpe_available <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(FALSE)
  tryCatch(isTRUE(reticulate::py_module_available("optuna")),
           error = function(e) FALSE)
}

#' Run the TPE optimizer on one objective from a shared initial design.
#'
#' Mirrors `run_bo()`'s contract so the result is directly comparable: the
#' returned `best` is a length-`budget + 1` best-so-far curve starting from the
#' initial design (index 1) and extended by one entry per TPE evaluation. The
#' search space matches the objective: continuous coordinates are suggested as
#' floats in [0, 1], and each categorical coordinate (per the objective's schema)
#' is suggested as one of its levels, then mapped back to the representative
#' unit-cube coordinate that `objective$fn` decodes to that level.
#'
#' @param objective    Objective list (`fn`, `d`, and optional `schema`).
#' @param cfg          Config list (uses `budget`).
#' @param X_init       Initial design (matrix in [0, 1]^d), shared with the others.
#' @param y_init       Objective values at `X_init`.
#' @param seed         Integer seed for the TPE sampler (reproducibility).
#' @param sampler_opts Named list of extra arguments forwarded to
#'   `optuna$samplers$TPESampler()` (e.g. `gamma`, `n_startup_trials`). Default
#'   `list()` reproduces Optuna's own defaults, i.e. today's behaviour.
#' @return A list with `best`: the best-so-far curve (length `budget + 1`).
run_tpe <- function(objective, cfg, X_init, y_init, seed, sampler_opts = list()) {
  optuna <- reticulate::import("optuna", delay_load = FALSE)
  optuna$logging$set_verbosity(optuna$logging$WARNING)   # quiet the study logs

  d      <- objective$d
  schema <- objective$schema
  types  <- if (is.null(schema)) rep("cont", d) else schema$types
  nlev   <- if (is.null(schema)) rep(NA_integer_, d) else schema$levels
  pname  <- paste0("x", seq_len(d))

  # One Optuna distribution per coordinate: a unit float for continuous inputs,
  # a categorical over 1..L for categorical inputs.
  dist_for <- function(j) {
    if (types[j] == "cat") {
      optuna$distributions$CategoricalDistribution(as.integer(seq_len(nlev[j])))
    } else {
      optuna$distributions$FloatDistribution(0, 1)
    }
  }

  # Convert a suggested parameter set into a [0, 1]^d row the objective accepts.
  # A categorical level k maps to (k - 0.5)/L, which decode_levels() sends back
  # to level k; a continuous float is used as-is.
  to_unit_row <- function(params) {
    u <- numeric(d)
    for (j in seq_len(d)) {
      pj <- as.numeric(params[[pname[j]]])
      u[j] <- if (types[j] == "cat") (pj - 0.5) / nlev[j] else pj
    }
    matrix(u, nrow = 1)
  }

  sampler <- do.call(optuna$samplers$TPESampler,
                      c(list(seed = as.integer(seed)), sampler_opts))
  study   <- optuna$create_study(direction = "minimize", sampler = sampler)

  # Seed the study with the shared initial design as completed trials, so TPE
  # starts from exactly the same observations as BASS-BO / GP-BO / Random.
  for (i in seq_len(nrow(X_init))) {
    params <- list(); dists <- list()
    for (j in seq_len(d)) {
      if (types[j] == "cat") {
        params[[pname[j]]] <- as.integer(decode_levels(X_init[i, j], nlev[j]))
      } else {
        params[[pname[j]]] <- as.numeric(X_init[i, j])
      }
      dists[[pname[j]]] <- dist_for(j)
    }
    study$add_trial(optuna$trial$create_trial(
      params = params, distributions = dists, value = as.numeric(y_init[i])))
  }

  best_so_far    <- numeric(cfg$budget + 1)
  best_so_far[1] <- min(y_init)

  # Ask-and-tell loop: propose a point, evaluate the (cheap, deterministic)
  # objective, report it back, and track the running best.
  for (t in seq_len(cfg$budget)) {
    trial  <- study$ask()
    params <- list()
    for (j in seq_len(d)) {
      params[[pname[j]]] <- if (types[j] == "cat") {
        trial$suggest_categorical(pname[j], as.integer(seq_len(nlev[j])))
      } else {
        trial$suggest_float(pname[j], 0, 1)
      }
    }
    y <- as.numeric(objective$fn(to_unit_row(params)))
    study$tell(trial, y)
    best_so_far[t + 1] <- min(best_so_far[t], y)
  }

  list(best = best_so_far)
}

#' Run the TPE baseline across all seeds, in the main process.
#'
#' Unlike the pure-R methods (which fan out across furrr workers), TPE is run
#' sequentially in the calling process: it is cheap relative to the BASS fits,
#' and a single Python session avoids the fragility of initializing reticulate
#' inside parallel workers. Each seed reproduces exactly the same initial design
#' as `run_one_seed()` (same `set.seed(seed)` and maximin LHS), so TPE starts
#' from the identical observations the other methods see.
#'
#' @param cfg Config list (uses `objective`, `d`, `budget`, `reps`, `seed_start`).
#' @return Long tibble with columns seed, iter, method ("TPE"), best.
run_tpe_experiment <- function(cfg) {
  objective <- load_objective(cfg$objective, cfg$d)
  d  <- objective$d
  n0 <- max(2 * d + 1, 8)
  seeds <- cfg$seed_start + 0:(cfg$reps - 1)

  curves <- lapply(seeds, function(seed) {
    set.seed(seed)
    X_init <- lhs::maximinLHS(n0, d)
    y_init <- objective$fn(X_init)
    tibble::tibble(
      seed   = seed,
      iter   = 0:cfg$budget,
      method = "TPE",
      best   = run_tpe(objective, cfg, X_init, y_init, seed)$best
    )
  })
  dplyr::bind_rows(curves)
}

#' Run TPE across all seeds for several sampler configurations.
#'
#' Companion to `run_tpe_experiment()` for a hyperparameter-sensitivity sweep:
#' instead of one TPE curve per seed, this produces one curve per
#' (seed, config) pair, with each config's `method` label distinguishing it in
#' the resulting long tibble. Every config sees the SAME initial design per
#' seed (same `set.seed(seed)` + maximin LHS), so the only thing that varies
#' across curves for a fixed seed is the sampler configuration.
#'
#' @param cfg     Config list (uses `objective`, `d`, `budget`, `reps`,
#'   `seed_start`), same as `run_tpe_experiment()`.
#' @param configs Named list; each element is a list with a `sampler_opts`
#'   entry (forwarded to `run_tpe()`). Names are used as the `method` label.
#' @return Long tibble with columns seed, iter, method (one of `names(configs)`),
#'   best.
run_tpe_sweep_experiment <- function(cfg, configs) {
  objective <- load_objective(cfg$objective, cfg$d)
  d  <- objective$d
  n0 <- max(2 * d + 1, 8)
  seeds <- cfg$seed_start + 0:(cfg$reps - 1)

  curves <- lapply(seeds, function(seed) {
    set.seed(seed)
    X_init <- lhs::maximinLHS(n0, d)
    y_init <- objective$fn(X_init)

    per_config <- lapply(names(configs), function(label) {
      sampler_opts <- configs[[label]]$sampler_opts
      tibble::tibble(
        seed   = seed,
        iter   = 0:cfg$budget,
        method = label,
        best   = run_tpe(objective, cfg, X_init, y_init, seed,
                          sampler_opts = sampler_opts)$best
      )
    })
    dplyr::bind_rows(per_config)
  })
  dplyr::bind_rows(curves)
}
