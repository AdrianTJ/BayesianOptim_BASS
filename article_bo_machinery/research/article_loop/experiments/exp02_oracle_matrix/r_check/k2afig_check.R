#!/usr/bin/env Rscript
# K2a-fig re-check, in R with the actual library (see E1/E2 ANALYSIS).
# Question: is the recorded "-0.148 at budget 10" (diagnostics README,
# pre-fix run) reproducible in R at n_cand=1000, and does it instead match a
# small-pool run? Arms: oracle + historical k-flip generator (reconstructed
# from commit 1ecc5a1) at n_cand 1000 and 50; oracle + current keep
# generator at n_cand 1000. func2C, seeds 1001-1010, budget 60.

suppressPackageStartupMessages(library(lhs))
lib <- "code_files/R"
source(file.path(lib, "bootstrap.R")); source_library(lib)

cfg <- default_config(); cfg$budget <- 60L

# Historical (pre-fix, commit 1ecc5a1) local move: k ~ U{1..min(3,n_cat)}
# coords flipped, >=1 always.
local_moves_hist <- function(X_local, x_best, schema, cat_idx) {
  n_cat <- length(cat_idx)
  inc <- vapply(cat_idx, function(j)
    as.integer(decode_levels(x_best[j], schema$levels[j])), integer(1))
  for (r in seq_len(nrow(X_local))) {
    k <- sample.int(min(3L, n_cat), 1L)
    flip <- sample.int(n_cat, k)
    for (c in seq_len(n_cat)) {
      j <- cat_idx[c]; L <- schema$levels[j]; lev <- inc[c]
      if (c %in% flip) lev <- sample(setdiff(seq_len(L), lev), 1L)
      X_local[r, j] <- (lev - 0.5) / L
    }
  }
  X_local
}

hist_candidates <- function(X_eval, y_eval, n_cand, schema) {
  X_eval <- as.matrix(X_eval); d <- ncol(X_eval)
  n_local <- floor(n_cand / 2); n_global <- n_cand - n_local
  X_global <- space_filling_candidates(n_global, d)
  bi <- which.min(y_eval); xb <- X_eval[bi, ]
  s <- local_scale(X_eval, bi)
  X_local <- matrix(rnorm(n_local * d, mean = rep(xb, each = n_local), sd = s), ncol = d)
  X_local <- pmin(pmax(X_local, 0), 1)
  ci <- which(schema$types == "cat")
  if (length(ci)) X_local <- local_moves_hist(X_local, xb, schema, ci)
  rbind(X_global, X_local)
}

objective <- load_objective("func2C", 4L)
schema <- objective$schema
seeds <- 1001:1010
res <- NULL
for (seed in seeds) {
  set.seed(seed)
  n0 <- max(2 * objective$d + 1, 8)
  X0 <- lhs::maximinLHS(n0, objective$d); y0 <- objective$fn(X0)
  arms <- list(
    hist1000 = list(name = "oracle+histflip-1000",
      candidates = function(X, y) hist_candidates(X, y, 1000L, schema),
      acquire = function(X, y, Xc) -objective$fn(Xc)),
    hist50 = list(name = "oracle+histflip-50",
      candidates = function(X, y) hist_candidates(X, y, 50L, schema),
      acquire = function(X, y, Xc) -objective$fn(Xc)),
    keep1000 = list(name = "oracle+keep-1000",
      candidates = function(X, y) hybrid_candidates(X, y, 1000L, schema),
      acquire = function(X, y, Xc) -objective$fn(Xc))
  )
  for (a in names(arms)) {
    r <- run_bo(objective, arms[[a]], cfg, X0, y0)
    res <- rbind(res, data.frame(seed = seed, arm = a,
                                 b10 = r$best[11], b60 = r$best[61]))
  }
  cat("seed", seed, "done\n")
}
write.csv(res, "article_bo_machinery/research/article_loop/experiments/exp02_oracle_matrix/r_check/r_results.csv", row.names = FALSE)
agg <- aggregate(cbind(b10, b60) ~ arm, res, mean)
print(agg)
cat("\nR-side verdict inputs: histflip-1000 mean b10 vs the recorded -0.148;\n")
cat("histflip-50 mean b10 tests the small-pool hypothesis.\n")
