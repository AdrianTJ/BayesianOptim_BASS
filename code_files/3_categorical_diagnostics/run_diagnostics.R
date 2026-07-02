#!/usr/bin/env Rscript

# =============================================================================
# run_diagnostics.R  --  Why doesn't BASS-BO beat Random on categorical tasks?
# =============================================================================
# Three diagnostics that separate the possible causes, from the outside in:
#
#   1) ORACLE CEILING (no BASS needed). Replace the acquisition with the true
#      objective (score = -f). The loop then always picks the best point in the
#      candidate pool, so the result is an upper bound on what ANY surrogate
#      could achieve with the current hybrid_candidates() generator. A second
#      arm uses a patched generator whose local rows may KEEP the incumbent's
#      categorical combination (the current one always flips >= 1 categorical
#      coordinate, so the continuous coordinates can never be refined at the
#      best known combination). If oracle+current barely beats Random, the
#      candidate pool -- not the surrogate -- is the bottleneck.
#
#   2) SURROGATE FIT QUALITY (needs BASS). Fit BASS on n random points of the
#      objective and measure Spearman rank correlation between the posterior
#      mean and the truth on held-out points, at the sample sizes the BO loop
#      actually sees (n0, n0+20, n0+80). If this is ~0, BASS cannot learn the
#      benchmark at thesis budgets and no acquisition will save it: the
#      benchmark is uninformative at this scale, not "BASS-BO is bad".
#
#   3) INSTRUMENTED BASS-BO RUNS (needs BASS). Run BASS-BO vs Random with
#      per-pick logging: was the pick a global (LHS) or local (Hamming)
#      candidate, and did its decoded categorical combination duplicate one
#      already evaluated (a wasted evaluation on a deterministic objective)?
#      Also an "easy mode" Cat-Ackley (d=3, L=5: 125 combinations) where a
#      correctly working BASS-BO MUST clearly beat Random -- a pass/fail
#      regression for the whole method.
#
# Usage:
#   Rscript code_files/3_categorical_diagnostics/run_diagnostics.R
#   Rscript code_files/3_categorical_diagnostics/run_diagnostics.R --reps=5 --budget=40
#
# Results are printed and written under results/diagnostics/.
# =============================================================================

suppressPackageStartupMessages({ library(lhs) })
has_bass <- requireNamespace("BASS", quietly = TRUE)

this_file  <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_dir <- if (length(this_file)) dirname(normalizePath(this_file)) else getwd()
lib_dir    <- normalizePath(file.path(script_dir, "..", "R"))
source(file.path(lib_dir, "bootstrap.R"))
source_library(lib_dir)

cfg <- default_config()
cfg$budget <- 60L
cfg$reps   <- 10L
cfg <- parse_cli_args(commandArgs(trailingOnly = TRUE), cfg)
out_dir <- file.path("results", "diagnostics")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

seeds <- cfg$seed_start + 0:(cfg$reps - 1)

# --- Shared helpers -----------------------------------------------------------

# Key a point by its decoded categorical levels (continuous coords rounded, so
# on purely categorical problems the key IS the combination).
combo_key <- function(x, schema) {
  paste(vapply(seq_along(x), function(j) {
    if (schema$types[j] == "cat") as.character(decode_levels(x[j], schema$levels[j]))
    else sprintf("%.4f", x[j])
  }, character(1)), collapse = "|")
}

shared_init <- function(objective, seed) {
  set.seed(seed)
  n0 <- max(2 * objective$d + 1, 8)
  X_init <- lhs::maximinLHS(n0, objective$d)
  list(X = X_init, y = objective$fn(X_init), n0 = n0)
}

# --- Patched local move: may keep the incumbent's combination -----------------
# Flip each categorical coordinate independently with prob 1/n_cat (so ~37% of
# local rows keep the full incumbent combination and refine only the continuous
# coordinates). The current generator always flips 1..3 coordinates.
local_categorical_moves_keep <- function(X_local, x_best, schema, cat_idx) {
  n_cat   <- length(cat_idx)
  inc_lev <- vapply(cat_idx,
                    function(j) as.integer(decode_levels(x_best[j], schema$levels[j])),
                    integer(1))
  for (r in seq_len(nrow(X_local))) {
    flip <- which(runif(n_cat) < 1 / n_cat)
    for (c in seq_len(n_cat)) {
      j   <- cat_idx[c]
      L   <- schema$levels[j]
      lev <- inc_lev[c]
      if (c %in% flip) lev <- sample(setdiff(seq_len(L), lev), 1L)
      X_local[r, j] <- (lev - 0.5) / L
    }
  }
  X_local
}

hybrid_candidates_keep <- function(X_eval, y_eval, n_cand, schema = NULL) {
  X_eval <- as.matrix(X_eval)
  d <- ncol(X_eval)
  n_local  <- floor(n_cand / 2)
  n_global <- n_cand - n_local
  X_global <- space_filling_candidates(n_global, d)
  best_idx <- which.min(y_eval)
  x_best   <- X_eval[best_idx, ]
  s        <- local_scale(X_eval, best_idx)
  X_local  <- matrix(rnorm(n_local * d, mean = rep(x_best, each = n_local), sd = s),
                     ncol = d)
  X_local  <- pmin(pmax(X_local, 0), 1)
  if (!is.null(schema) && n_local > 0) {
    cat_idx <- which(schema$types == "cat")
    if (length(cat_idx)) {
      X_local <- local_categorical_moves_keep(X_local, x_best, schema, cat_idx)
    }
  }
  rbind(X_global, X_local)
}

# =============================================================================
# 1) ORACLE CEILING
# =============================================================================
cat("==========================================================\n")
cat("1) Oracle ceiling: candidate pool quality, surrogate-free\n")
cat("==========================================================\n")

oracle_rows <- list()
for (obj_spec in list(list(name = "func2C", d = 4L),
                      list(name = "func3C", d = 5L),
                      list(name = "cat_ackley", d = 6L))) {
  finals <- t(vapply(seeds, function(seed) {
    objective <- load_objective(obj_spec$name, obj_spec$d)
    schema    <- objective$schema
    o_now <- list(name = "oracle+current",
      candidates = function(X, y) hybrid_candidates(X, y, cfg$n_cand, schema),
      acquire    = function(X, y, Xc) -objective$fn(Xc))
    o_fix <- list(name = "oracle+keep-combo",
      candidates = function(X, y) hybrid_candidates_keep(X, y, cfg$n_cand, schema),
      acquire    = function(X, y, Xc) -objective$fn(Xc))
    rnd   <- list(name = "Random", candidates = NULL, acquire = NULL)

    init <- shared_init(objective, seed)
    c(current = tail(run_bo(objective, o_now, cfg, init$X, init$y)$best, 1),
      keep    = tail(run_bo(objective, o_fix, cfg, init$X, init$y)$best, 1),
      random  = tail(run_bo(objective, rnd,   cfg, init$X, init$y)$best, 1))
  }, numeric(3)))

  cat(sprintf("\n%s (budget=%d, reps=%d) -- mean final best (lower is better):\n",
              obj_spec$name, cfg$budget, cfg$reps))
  cat(sprintf("  oracle + current generator   : %9.4f\n", mean(finals[, "current"])))
  cat(sprintf("  oracle + keep-combo generator: %9.4f\n", mean(finals[, "keep"])))
  cat(sprintf("  Random                       : %9.4f\n", mean(finals[, "random"])))
  cat(sprintf("  paired wins: keep<current %d/%d | current<random %d/%d\n",
              sum(finals[, "keep"] < finals[, "current"]), nrow(finals),
              sum(finals[, "current"] < finals[, "random"]), nrow(finals)))
  oracle_rows[[obj_spec$name]] <- data.frame(objective = obj_spec$name,
                                             seed = seeds, finals)
}
write.csv(do.call(rbind, oracle_rows),
          file.path(out_dir, "oracle_ceiling.csv"), row.names = FALSE)

if (!has_bass) {
  cat("\nBASS is not installed; skipping diagnostics 2 and 3.\n")
  quit(save = "no")
}

# =============================================================================
# 2) SURROGATE FIT QUALITY AT BO SAMPLE SIZES
# =============================================================================
cat("\n==========================================================\n")
cat("2) BASS fit quality at the sample sizes the BO loop sees\n")
cat("==========================================================\n")

fit_quality <- function(objective, n_fit, seed) {
  schema <- objective$schema
  if (!is.null(schema)) .ensure_bass_cat_predict_fix()
  set.seed(seed)
  d <- objective$d
  X <- matrix(runif(n_fit * d), ncol = d)
  y <- objective$fn(X)
  y_std <- (y - mean(y)) / max(sd(y), 1e-12)
  fit <- BASS::bass(xx = to_model_frame(X, schema), y = y_std,
                    nmcmc = BASS_NMCMC, nburn = BASS_NBURN, thin = BASS_THIN,
                    verbose = FALSE)
  X_test <- matrix(runif(500 * d), ncol = d)
  y_test <- objective$fn(X_test)
  draws  <- .samples_by_cand(predict(fit, to_model_frame(X_test, schema),
                                     mcmc.use = seq_len(BASS_KEEP)), 500L)
  pm <- colMeans(draws)
  c(spearman = cor(pm, y_test, method = "spearman"),
    nbasis   = mean(fit$nbasis))
}

fq_rows <- list()
for (obj_spec in list(list(name = "func2C", d = 4L),
                      list(name = "func3C", d = 5L),
                      list(name = "cat_ackley", d = 6L))) {
  objective <- load_objective(obj_spec$name, obj_spec$d)
  n0 <- max(2 * objective$d + 1, 8)
  for (n_fit in c(n0, n0 + 20L, n0 + cfg$budget)) {
    r <- t(vapply(seeds[seq_len(min(5, length(seeds)))],
                  function(s) fit_quality(objective, n_fit, s), numeric(2)))
    cat(sprintf("  %-10s n=%3d : held-out Spearman(pred, truth) = %5.2f (+/- %.2f), mean #basis = %4.1f\n",
                obj_spec$name, n_fit, mean(r[, "spearman"]), sd(r[, "spearman"]),
                mean(r[, "nbasis"])))
    fq_rows[[paste(obj_spec$name, n_fit)]] <-
      data.frame(objective = obj_spec$name, n_fit = n_fit,
                 spearman = r[, "spearman"], nbasis = r[, "nbasis"])
  }
}
write.csv(do.call(rbind, fq_rows),
          file.path(out_dir, "bass_fit_quality.csv"), row.names = FALSE)

# =============================================================================
# 3) INSTRUMENTED BASS-BO vs RANDOM (paired per seed)
# =============================================================================
cat("\n==========================================================\n")
cat("3) Instrumented BASS-BO runs (pick provenance + revisits)\n")
cat("==========================================================\n")

# Same loop as run_bo(), plus per-pick logging. Kept local to the diagnostic so
# the library stays untouched.
run_bo_logged <- function(objective, method, cfg, X_init, y_init) {
  f      <- objective$fn
  schema <- objective$schema
  X_eval <- as.matrix(X_init)
  y_eval <- as.numeric(y_init)
  n_global <- cfg$n_cand - floor(cfg$n_cand / 2)

  seen <- vapply(seq_len(nrow(X_eval)),
                 function(i) combo_key(X_eval[i, ], schema), character(1))
  best_so_far    <- numeric(cfg$budget + 1)
  best_so_far[1] <- min(y_eval)
  origin  <- character(cfg$budget)
  revisit <- logical(cfg$budget)

  for (t in 1:cfg$budget) {
    X_cand <- method$candidates(X_eval, y_eval)
    score  <- method$acquire(X_eval, y_eval, X_cand)
    score[min_sqdist(X_cand, X_eval) <= cfg$dup_tol^2] <- -Inf
    pick   <- which.max(score)
    x_next <- X_cand[pick, , drop = FALSE]

    origin[t]  <- if (pick <= n_global) "global" else "local"
    k          <- combo_key(x_next[1, ], schema)
    revisit[t] <- k %in% seen
    seen       <- c(seen, k)

    y_next <- f(x_next)
    X_eval <- rbind(X_eval, x_next)
    y_eval <- c(y_eval, y_next)
    best_so_far[t + 1] <- min(y_eval)
  }
  list(best = best_so_far, origin = origin, revisit = revisit)
}

instrument_case <- function(obj_name, make_objective, budget) {
  cfg_case <- cfg; cfg_case$budget <- budget
  rows <- lapply(seeds, function(seed) {
    objective <- make_objective()
    methods   <- make_methods(cfg_case, objective$schema)
    init      <- shared_init(objective, seed)
    rb <- run_bo_logged(objective, methods[["BASS-BO"]], cfg_case, init$X, init$y)
    rr <- run_bo(objective, methods[["Random"]], cfg_case, init$X, init$y)
    data.frame(seed = seed,
               bass_final = tail(rb$best, 1), random_final = tail(rr$best, 1),
               n_local = sum(rb$origin == "local"),
               n_revisit = sum(rb$revisit))
  })
  df <- do.call(rbind, rows)
  cat(sprintf("\n%s (budget=%d, reps=%d):\n", obj_name, budget, cfg$reps))
  cat(sprintf("  mean final best: BASS-BO %9.4f | Random %9.4f\n",
              mean(df$bass_final), mean(df$random_final)))
  cat(sprintf("  paired wins BASS<Random: %d/%d (shared initial design => paired)\n",
              sum(df$bass_final < df$random_final), nrow(df)))
  if (requireNamespace("stats", quietly = TRUE) && nrow(df) >= 6) {
    p <- tryCatch(stats::wilcox.test(df$bass_final, df$random_final,
                                     paired = TRUE)$p.value, error = function(e) NA)
    cat(sprintf("  Wilcoxon signed-rank (paired) p = %.3f\n", p))
  }
  cat(sprintf("  BASS-BO picks: %.0f%% local-half | revisited combos: %.1f of %d picks\n",
              100 * mean(df$n_local / budget), mean(df$n_revisit), budget))
  df$objective <- obj_name
  df
}

easy_cat_ackley <- function() {
  ca <- make_cat_ackley(3L, L = 5L)   # 125 combinations: budget covers a third
  list(name = "cat_ackley_easy", d = ca$d, fn = ca$fn, schema = ca$schema)
}

inst <- rbind(
  # Easy mode: 125 combinations, a working BASS-BO MUST clearly beat Random.
  instrument_case("cat_ackley_easy (d=3, L=5)", easy_cat_ackley, min(cfg$budget, 40L)),
  instrument_case("func2C", function() load_objective("func2C", 4L), cfg$budget),
  instrument_case("cat_ackley (d=6, L=11)",
                  function() load_objective("cat_ackley", 6L), cfg$budget)
)
write.csv(inst, file.path(out_dir, "instrumented_runs.csv"), row.names = FALSE)

cat(sprintf("\nAll diagnostic artifacts saved under: %s\n", normalizePath(out_dir)))
