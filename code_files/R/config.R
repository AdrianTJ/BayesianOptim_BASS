# =============================================================================
# config.R  --  One place for every tunable knob
# =============================================================================
# Previously each runner re-declared the same ~20 settings and its own copy of
# the `--key=value` parser. Here we declare the defaults once and parse CLI
# overrides generically, so adding a knob to `default_config()` automatically
# makes it settable from the command line.
# =============================================================================

#' Default experiment configuration.
#'
#' Returns a flat named list. Anything in here can be overridden on the command
#' line as `--name=value` (see `parse_cli_args()`).
#'
#' @return Named list of settings.
default_config <- function() {
  list(
    # ---- Experiment ----
    objective  = "branin",  # which objective to optimise (see objectives/)
    d          = 2,         # input dimension
    budget     = 80,        # BO iterations after the initial design
    n_cand     = 1000,      # candidate points scored per iteration
    reps       = 10,        # independent repetitions (seeds)
    seed_start = 1001,      # first seed; reps use seed_start + 0..reps-1
    out_dir    = "results", # where CSVs and the plot are written

    # ---- Acquisition / numerics shared across methods ----
    kappa       = 2.0,      # fixed LCB weight for the GP baseline
    eps         = 1e-10,    # jitter added to GP predictive variance
    dup_tol     = 1e-10,    # distance below which points count as duplicates

    # ---- BASS-specific tuning ----
    bass_kappa_start  = 3.5,  # LCB weight at the first iteration (explore)
    bass_kappa_end    = 1.5,  # LCB weight at the last iteration (exploit)
    bass_sd_floor     = 1e-3, # minimum posterior SD (prevents overconfidence)
    bass_sd_inflate   = 1.20, # multiplier on posterior SD
    bass_local_frac   = 0.35, # fraction of candidates drawn near the best point
    bass_local_sd     = 0.08, # spread of that local candidate cloud
    explore_every     = 7,    # take a pure-exploration step every Nth iteration
    bass_degree_early = 1,    # spline degree before `bass_switch_after`
    bass_degree_late  = 2,    # spline degree after `bass_switch_after`
    bass_switch_after = 40    # point count at which we raise the spline degree
  )
}

#' Apply `--key=value` command-line overrides to a config list.
#'
#' Only keys already present in `cfg` are accepted (a typo'd flag errors rather
#' than silently doing nothing). Values are coerced to match the type of the
#' existing default.
#'
#' @param args Character vector, e.g. from `commandArgs(trailingOnly = TRUE)`.
#' @param cfg  Base config to override (defaults to `default_config()`).
#' @return The updated config list.
parse_cli_args <- function(args, cfg = default_config()) {
  for (a in args) {
    if (!grepl("^--[^=]+=", a)) {
      stop(sprintf("Malformed argument '%s'; expected --key=value", a))
    }
    key <- sub("^--([^=]+)=.*$", "\\1", a)
    val <- sub("^--[^=]+=", "", a)

    if (!key %in% names(cfg)) {
      stop(sprintf("Unknown setting '--%s'. Valid keys: %s",
                   key, paste(names(cfg), collapse = ", ")))
    }

    # Coerce the string to whatever type the default uses.
    default <- cfg[[key]]
    cfg[[key]] <- if (is.integer(default)) {
      as.integer(val)
    } else if (is.numeric(default)) {
      as.numeric(val)
    } else if (is.logical(default)) {
      tolower(val) %in% c("1", "true", "t", "yes", "y")
    } else {
      val  # character
    }
  }
  cfg
}
