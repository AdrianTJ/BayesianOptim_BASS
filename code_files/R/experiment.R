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
#' @param seed      Integer seed (controls the initial design and the run).
#' @param objective Objective list from `load_objective()`.
#' @param methods   Named list of methods from `make_methods()`.
#' @param cfg       Config list.
#' @return Long tibble with columns: seed, iter, method, best.
run_one_seed <- function(seed, objective, methods, cfg) {
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
#' @param objective Objective list from `load_objective()`.
#' @param methods   Named list of methods from `make_methods()`.
#' @param cfg       Config list (uses `reps` and `seed_start`).
#' @return Long tibble of every method's best-so-far curve for every seed.
run_experiment <- function(objective, methods, cfg) {
  seeds <- cfg$seed_start + 0:(cfg$reps - 1)
  furrr::future_map_dfr(
    seeds,
    ~ run_one_seed(.x, objective, methods, cfg),
    # `packages` ensures each parallel worker loads the surrogate namespaces so
    # that predict() dispatches to predict.bass / predict.GP correctly.
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
