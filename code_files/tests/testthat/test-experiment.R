# Tests for the parallel experiment driver (run_experiment / run_one_seed).
#
# Regression guard for the bug where fanning seeds out across furrr workers died
# with "could not find function ..." (scale01_to_bounds, then run_one_seed). The
# cause: objective/method closures built in the main process were passed to the
# workers as data, and future's globals detection cannot see helpers that are
# only reachable *through* those closures. run_one_seed now rebuilds the objective
# and methods from cfg on each worker, keeping every dependency reachable.
#
# These tests use plan(multisession) on purpose: those workers are fresh R
# sessions with an empty global environment, so they actually reproduce the
# failure. A forked (multicore) worker would inherit the parent's globals and
# silently mask it.

test_that("a rebuilt benchmark objective evaluates on a fresh parallel worker", {
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  skip_if_not_installed("lhs")

  old_plan <- future::plan(future::multisession, workers = 2)
  on.exit(future::plan(old_plan), add = TRUE)

  # Build the objective *inside* the worker, exactly as run_one_seed does. This
  # exercises the load_objective -> vectorize_target -> scale01_to_bounds chain:
  # all three must be reachable by future's code inspection, or objective$fn
  # would die on the worker with a missing-function error.
  out <- furrr::future_map_dbl(
    1:2,
    function(s) {
      obj <- load_objective("branin", 2)
      set.seed(s)
      min(obj$fn(lhs::randomLHS(6, 2)))
    },
    .options = furrr::furrr_options(seed = TRUE, packages = "lhs")
  )

  expect_length(out, 2)
  expect_true(all(is.finite(out)))
})

test_that("run_experiment fans seeds across real workers end to end (if installed)", {
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  skip_if_not_installed("lhs")
  skip_if_not_installed("BASS")
  skip_if_not_installed("GPfit")

  old_plan <- future::plan(future::multisession, workers = 2)
  on.exit(future::plan(old_plan), add = TRUE)

  # A miniature of the failing CLI command:
  #   Rscript run_benchmark.R --objective=branin --d=2 --budget=80 --reps=25
  # branin's objective hides scale01_to_bounds in objective$fn, and the BASS/GP
  # methods hide ei_mc / ei_gaussian / the BASS_* constants in method$acquire --
  # all the helpers that previously failed to reach the workers.
  cfg <- default_config()
  cfg$objective   <- "branin"
  cfg$d           <- 2
  cfg$budget      <- 2
  cfg$n_cand      <- 50
  cfg$reps        <- 2
  cfg$acquisition <- "ei"

  runs <- run_experiment(cfg)

  methods <- c("BASS-BO", "GP-BO", "Random")
  expect_setequal(unique(runs$method), methods)
  # One row per (seed, method, iteration 0..budget).
  expect_equal(nrow(runs), cfg$reps * length(methods) * (cfg$budget + 1))
  expect_true(all(is.finite(runs$best)))
  # Best-so-far must be non-increasing within every (seed, method) curve.
  ok <- tapply(seq_len(nrow(runs)), list(runs$seed, runs$method), function(ix) {
    all(diff(runs$best[ix][order(runs$iter[ix])]) <= 0)
  })
  expect_true(all(ok))
})
