# =============================================================================
# config.R  --  One place for every tunable knob
# =============================================================================
# After moving to Expected Improvement the tuning surface is tiny: the BASS loop
# no longer has ANY method-specific knobs (no kappa schedule, no sd inflation,
# no degree switching, no local-sampling parameters). What remains is just the
# experiment setup plus one acquisition choice.
#
# Defaults are declared once here; CLI overrides are parsed generically, so
# adding a setting automatically makes it settable as --name=value.
# =============================================================================

#' Default experiment configuration.
#'
#' @return Named list of settings (all overridable via `--name=value`).
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

    # ---- Acquisition ----
    # BASS acquisition: "ei" (Monte Carlo Expected Improvement, the principled
    # default) or "thompson" (single-draw Thompson sampling, fastest). The GP
    # baseline always uses closed-form EI.
    acquisition = "ei",

    # ---- Numerics (hygiene, not tuning) ----
    eps     = 1e-10,        # jitter added to the GP predictive variance
    dup_tol = 1e-10         # distance below which points count as duplicates
  )
}

#' Apply `--key=value` command-line overrides to a config list.
#'
#' Only keys already present in `cfg` are accepted (a typo'd flag errors rather
#' than silently doing nothing). Values are coerced to the type of the default.
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
