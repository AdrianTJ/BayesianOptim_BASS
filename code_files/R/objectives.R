# =============================================================================
# objectives.R  --  Turn an objective name into a ready-to-optimise objective
# =============================================================================
# The BO loop expects an objective as:
#     list(name, d, fn)
# where `fn` maps an n x d matrix in [0, 1]^d to n values to be MINIMISED.
#
# Benchmark functions (Branin, Rastrigin) are defined on their own physical
# domains, so we wrap them with vectorize_target() to (a) rescale [0,1]^d to the
# real domain and (b) accept matrix input. The synthetic surface already lives
# on [0, 1]^d, so it is used as-is.
#
# This file assumes the individual objective definitions in R/objectives/ have
# already been sourced (the entry-point runner does that via source_library()).
# =============================================================================

#' Load a named objective at a given dimension.
#'
#' @param name One of "branin", "rastrigin", "synthetic".
#' @param d    Input dimension. Branin is fixed at 2; Rastrigin and synthetic
#'             accept any d >= 1 (Rastrigin's box [-5.12, 5.12] is repeated per
#'             dimension).
#' @return     A `list(name, d, fn)` objective.
load_objective <- function(name, d) {
  fn <- switch(name,
    "branin" = {
      if (d != 2) stop("Branin is only defined for d = 2.")
      vectorize_target(branin, branin_bounds)
    },
    "rastrigin" = {
      bounds <- list(lower = rep(-5.12, d), upper = rep(5.12, d))
      vectorize_target(rastrigin, bounds)
    },
    # Already on [0, 1]^d -- used directly, no rescaling needed.
    "synthetic" = synthetic,
    stop(sprintf("Unknown objective '%s'. Choose: branin, rastrigin, synthetic.",
                 name))
  )

  list(name = name, d = d, fn = fn)
}
