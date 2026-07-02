# =============================================================================
# objectives.R  --  Turn an objective name into a ready-to-optimise objective
# =============================================================================
# The BO loop expects an objective as:
#     list(name, d, fn, schema)
# where `fn` maps an n x d matrix in [0, 1]^d to n values to be MINIMISED, and
# `schema` (NULL for the purely continuous benchmarks) marks which coordinates
# are categorical so the BASS surrogate can use factors (see objective_utils.R).
#
# Continuous benchmarks (Branin, Rastrigin) are defined on their own physical
# domains, so we wrap them with vectorize_target() to (a) rescale [0,1]^d to the
# real domain and (b) accept matrix input. The synthetic surface already lives
# on [0, 1]^d. The categorical/mixed benchmarks (func2C, func3C, cat_ackley) take
# [0,1]^d directly and decode their own categorical coordinates.
#
# This file assumes the individual objective definitions in R/objectives/ have
# already been sourced (the entry-point runner does that via source_library()).
# =============================================================================

#' Load a named objective at a given dimension.
#'
#' @param name  One of "branin", "rastrigin", "synthetic", "func2C", "func3C",
#'              "cat_ackley".
#' @param d     Input dimension. Branin is fixed at 2; Rastrigin and synthetic
#'              accept any d >= 1. func2C/func3C have a fixed structure (d = 4 / 5)
#'              and ignore `d` with a warning if it disagrees; cat_ackley uses `d`
#'              as the number of categorical inputs.
#' @param cat_L Levels per categorical input, cat_ackley only (odd; see
#'              make_cat_ackley). NULL falls back to the classic 11, so the size
#'              of the categorical space (L^d) is a protocol choice: d=3/L=5 is
#'              solvable within a thesis budget, d=6/L=11 is the hard regime.
#' @return      A `list(name, d, fn, schema)` objective. `schema` is NULL for the
#'              continuous benchmarks.
load_objective <- function(name, d, cat_L = NULL) {
  if (is.null(cat_L)) cat_L <- 11L
  # Continuous benchmarks: a function plus a NULL schema.
  cont <- function(fn) list(name = name, d = d, fn = fn, schema = NULL)

  # Fixed-structure mixed benchmark: warn if the caller's d disagrees, then use
  # the benchmark's own dimension.
  fixed <- function(fn, schema, nat_d) {
    if (!is.null(d) && d != nat_d) {
      warning(sprintf("'%s' has a fixed structure (d = %d); ignoring d = %d.",
                      name, nat_d, d))
    }
    list(name = name, d = nat_d, fn = fn, schema = schema)
  }

  switch(name,
    "branin" = {
      if (d != 2) stop("Branin is only defined for d = 2.")
      cont(vectorize_target(branin, branin_bounds))
    },
    "rastrigin" = cont(vectorize_target(
      rastrigin, list(lower = rep(-5.12, d), upper = rep(5.12, d)))),
    # Already on [0, 1]^d -- used directly, no rescaling needed.
    "synthetic" = cont(synthetic),

    # --- Categorical / mixed benchmarks --------------------------------------
    "func2C" = fixed(
      func2C,
      list(types = c("cat", "cat", "cont", "cont"), levels = c(3L, 5L, NA, NA)),
      nat_d = 4L),
    "func3C" = fixed(
      func3C,
      list(types = c("cat", "cat", "cat", "cont", "cont"),
           levels = c(3L, 5L, 4L, NA, NA)),
      nat_d = 5L),
    "cat_ackley" = {
      ca <- make_cat_ackley(d, L = as.integer(cat_L))
      list(name = name, d = ca$d, fn = ca$fn, schema = ca$schema,
           opt_levels = ca$opt_levels)
    },

    stop(sprintf(paste0("Unknown objective '%s'. Choose: branin, rastrigin, ",
                        "synthetic, func2C, func3C, cat_ackley."), name))
  )
}
