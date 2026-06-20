# =============================================================================
# experiment.R  --  Run many seeds in parallel and summarise the results
# =============================================================================
# Wraps the BO loop in the repeated-experiment machinery: for each random seed
# we build one shared initial design, run all methods on it, and collect the
# best-so-far curves. Seeds are independent, so we fan them out across cores
# with future/furrr. This logic used to live (duplicated) inside each runner.
# =============================================================================

#' Run every method once for a single seed, from a shared initial design.
#'
#' The objective and the methods are (re)built here from `cfg` rather than passed
#' in. This is deliberate and load-bearing for the parallel path: when
#' `run_experiment()` fans this out across furrr workers, future's globals
#' detection can only export helpers it reaches by *inspecting function bodies*.
#' Helpers hidden inside closures stored as list values -- e.g. `scale01_to_bounds`
#' inside `objective$fn`, or `ei_mc` / the `BASS_*` constants inside
#' `method$acquire` -- are invisible to that inspection. Building the objective and
#' methods here keeps every dependency reachable by a chain of named calls
#' (`run_one_seed` -> `load_objective` / `make_methods` -> ...), so each worker
#' receives a complete, self-contained task. (This mirrors the elastic-net case
#' study, which builds its methods inside the per-seed worker function too.)
#'
#' @param seed Integer seed (controls the initial design and the run).
#' @param cfg  Config list (carries the objective name + dimension and the
#'             method/acquisition knobs).
#' @return Long tibble with columns: seed, iter, method, best.
run_one_seed <- function(seed, cfg) {
  objective <- load_objective(cfg$objective, cfg$d)
  methods   <- make_methods(cfg)

  set.seed(seed)
  d  <- objective$d
  # A modest space-filling start: ~2d+1 points, but at least 8.
  n0 <- max(2 * d + 1, 8)
  X_init <- lhs::maximinLHS(n0, d)
  y_init <- objective$fn(X_init)

  # Run each method on the SAME starting design for a fair comparison.
  curves <- lapply(methods, function(m) {
    tibble::tibble(
      seed   = seed,
      iter   = 0:cfg$budget,
      method = m$name,
      best   = run_bo(objective, m, cfg, X_init, y_init)$best
    )
  })
  dplyr::bind_rows(curves)
}

#' Run the full experiment across all seeds, in parallel.
#'
#' Each worker rebuilds the objective and methods from `cfg` (see `run_one_seed`),
#' which keeps every user-defined helper reachable by future's globals detection,
#' so no explicit `globals` list is needed. `packages` is still required: it loads
#' the surrogate namespaces on each worker so that `predict()` dispatches to
#' `predict.bass` / `predict.GP` correctly.
#'
#' @param cfg Config list (uses `objective`, `d`, `reps`, `seed_start`, and the
#'            method/acquisition knobs consumed downstream).
#' @return Long tibble of every method's best-so-far curve for every seed.
run_experiment <- function(cfg) {
  seeds <- cfg$seed_start + 0:(cfg$reps - 1)
  furrr::future_map_dfr(
    seeds,
    ~ run_one_seed(.x, cfg),
    .options = furrr::furrr_options(
      seed = TRUE,
      packages = c("BASS", "GPfit", "lhs")
    )
  )
}

#' Aggregate raw runs into a per-iteration convergence summary.
#'
#' @param all_runs Long tibble from `run_experiment()`.
#' @return Tibble with mean/SD of best-so-far per (method, iter) plus a 95% CI.
summarise_curve <- function(all_runs) {
  all_runs |>
    dplyr::group_by(method, iter) |>
    dplyr::summarise(
      mean_best = mean(best),
      sd_best   = sd(best),
      n         = dplyr::n(),
      .groups   = "drop"
    ) |>
    dplyr::mutate(
      se      = sd_best / sqrt(n),
      ci_low  = mean_best - 1.96 * se,
      ci_high = mean_best + 1.96 * se
    )
}

#' Aggregate raw runs into a final-performance leaderboard.
#'
#' @param all_runs Long tibble from `run_experiment()`.
#' @return Tibble with the mean/SD of each method's final best, best first.
summarise_final <- function(all_runs) {
  all_runs |>
    dplyr::group_by(seed, method) |>
    dplyr::filter(iter == max(iter)) |>
    dplyr::ungroup() |>
    dplyr::group_by(method) |>
    dplyr::summarise(
      mean_final = mean(best),
      sd_final   = sd(best),
      .groups    = "drop"
    ) |>
    dplyr::arrange(mean_final)
}

#' Write all CSVs and the convergence plot for an experiment.
#'
#' Produces, in `cfg$out_dir`: all_runs.csv, summary_curve.csv,
#' final_summary.csv and convergence_mean_ci.png.
#'
#' @param all_runs  Long tibble from `run_experiment()`.
#' @param objective Objective list (used for plot labels).
#' @param cfg       Config list (uses `out_dir`, `reps`, `d`).
#' @return The final-performance tibble (invisibly handy for printing).
save_results <- function(all_runs, objective, cfg) {
  dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)

  summary_curve <- summarise_curve(all_runs)
  final_summary <- summarise_final(all_runs)

  readr::write_csv(all_runs,      file.path(cfg$out_dir, "all_runs.csv"))
  readr::write_csv(summary_curve, file.path(cfg$out_dir, "summary_curve.csv"))
  readr::write_csv(final_summary, file.path(cfg$out_dir, "final_summary.csv"))

  p <- ggplot2::ggplot(
    summary_curve,
    ggplot2::aes(x = iter, y = mean_best, color = method, fill = method)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_low, ymax = ci_high),
                         alpha = 0.15, linewidth = 0) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title    = sprintf("Mean convergence: %s (d = %d)",
                         objective$name, objective$d),
      subtitle = sprintf("Across %d seeds, 95%% CI", cfg$reps),
      x = "Iteration (after initialisation)",
      y = "Best objective so far (lower is better)"
    ) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(
    filename = file.path(cfg$out_dir, "convergence_mean_ci.png"),
    plot = p, width = 8, height = 4, dpi = 150
  )

  final_summary
}
