# Tests for the categorical / mixed-variable benchmarks and the encoding helpers
# that let BASS fit them as factors. These need no BASS/GPfit: they check the
# objective math, the unit-cube -> level decoding, the BASS model frame, and that
# the generic loop runs on a categorical objective (via a non-BASS oracle).

# --- Encoding helpers --------------------------------------------------------

test_that("decode_levels bins [0,1] into 1..L and keeps the right endpoint", {
  expect_equal(decode_levels(c(0, 0.49, 0.5, 0.99, 1), 2L), c(1L, 1L, 2L, 2L, 2L))
  expect_equal(decode_levels(seq(0, 1, length.out = 5), 5L), c(1L, 2L, 3L, 4L, 5L))
  # Every level is reachable and nothing escapes to L + 1.
  u <- runif(1000)
  lv <- decode_levels(u, 7L)
  expect_true(all(lv >= 1L & lv <= 7L))
  expect_setequal(unique(lv), 1:7)
})

test_that("to_model_frame makes categorical columns factors with full levels", {
  schema <- list(types = c("cat", "cat", "cont", "cont"), levels = c(3L, 5L, NA, NA))
  X  <- rbind(c(0.0, 0.0, 0.1, 0.9),
              c(0.9, 0.9, 0.5, 0.2))
  df <- to_model_frame(X, schema)

  expect_s3_class(df[[1]], "factor")
  expect_s3_class(df[[2]], "factor")
  expect_false(is.factor(df[[3]]))
  expect_false(is.factor(df[[4]]))
  # Factors advertise ALL levels even though only some appear in these 2 rows.
  expect_equal(levels(df[[1]]), as.character(1:3))
  expect_equal(levels(df[[2]]), as.character(1:5))
  # Continuous columns pass through untouched.
  expect_equal(df[[3]], X[, 3])
})

# --- CoCaBO Func-2C / Func-3C ------------------------------------------------

test_that("func2C matches the CoCaBO reference arithmetic", {
  # Row: h1 -> level 1 (Rosenbrock), h2 -> level 1 (Rosenbrock), x1 = x2 = 0.
  # x = 4*0.5 - 2 = 0; Rosenbrock(0,0)/300 = (0 + 1)/300. Two equal terms.
  X <- matrix(c(0, 0, 0.5, 0.5), nrow = 1)
  expect_equal(func2C(X), 2 / 300, tolerance = 1e-12)

  # h2 -> level 3 selects Beale (the CoCaBO `else` branch).
  X2 <- matrix(c(0, 0.5, 0.5, 0.5), nrow = 1)   # h1=1 Rosenbrock, h2=3 Beale
  expect_equal(func2C(X2),
               .cocabo_rosenbrock(0, 0) + .cocabo_beale(0, 0),
               tolerance = 1e-12)

  expect_length(func2C(matrix(runif(4 * 5), ncol = 4)), 5)
})

test_that("func3C adds the weighted third categorical term", {
  # h1=1 Rosenbrock, h2=1 Rosenbrock, h3=4 -> (h3-1)=3 x Beale; x1=x2=0.
  X <- matrix(c(0, 0, 0.99, 0.5, 0.5), nrow = 1)
  expect_equal(func3C(X),
               2 * .cocabo_rosenbrock(0, 0) + 3 * .cocabo_beale(0, 0),
               tolerance = 1e-12)
  expect_length(func3C(matrix(runif(5 * 4), ncol = 5)), 4)
})

# --- Permuted-categorical Ackley ---------------------------------------------

test_that("cat_ackley reaches its known global minimum of 0", {
  d  <- 5L
  ca <- make_cat_ackley(d, L = 11L, seed = 1L)

  # Place each input on the level the permutation maps to grid value 0.
  L <- 11L
  u_opt <- (ca$opt_levels - 0.5) / L
  expect_equal(decode_levels(u_opt, L), ca$opt_levels)   # sanity: u -> opt level
  expect_equal(ca$fn(matrix(u_opt, nrow = 1)), 0, tolerance = 1e-9)

  # Away from the optimum the value is strictly positive.
  expect_gt(ca$fn(matrix(runif(d), nrow = 1)), 0)

  expect_equal(ca$schema$types, rep("cat", d))
  expect_equal(ca$schema$levels, rep(11L, d))
})

test_that("the fixed permutation does not disturb the global RNG stream", {
  set.seed(123)
  a <- runif(3)
  set.seed(123)
  invisible(make_cat_ackley(4))   # builds permutations internally
  b <- runif(3)
  expect_equal(a, b)              # RNG state was preserved
})

# --- load_objective wiring ---------------------------------------------------

test_that("load_objective returns schema + fixed dimensions for the new benchmarks", {
  o2 <- load_objective("func2C", 4)
  expect_equal(o2$d, 4L)
  expect_equal(o2$schema$types, c("cat", "cat", "cont", "cont"))

  o3 <- load_objective("func3C", 5)
  expect_equal(o3$d, 5L)
  expect_equal(o3$schema$levels, c(3L, 5L, 4L, NA, NA))

  oc <- load_objective("cat_ackley", 6)
  expect_equal(oc$d, 6)
  expect_true(all(oc$schema$types == "cat"))
  expect_equal(oc$schema$levels, rep(11L, 6))   # classic default preserved

  # cat_L controls the instance size (the easy/medium/hard protocol knob).
  oe <- load_objective("cat_ackley", 3, cat_L = 5L)
  expect_equal(oe$schema$levels, rep(5L, 3))
  expect_equal(oe$fn(matrix((oe$opt_levels - 0.5) / 5, nrow = 1)), 0,
               tolerance = 1e-9)

  # Continuous benchmarks still carry a NULL schema.
  expect_null(load_objective("branin", 2)$schema)

  # Fixed-structure benchmark warns on a mismatched d but still works.
  expect_warning(load_objective("func2C", 2), "fixed structure")
})

# --- End-to-end loop on a categorical objective (no BASS needed) -------------

test_that("the generic BO loop runs on a categorical objective via an oracle", {
  set.seed(99)
  obj <- load_objective("func2C", 4)

  cfg <- default_config()
  cfg$budget <- 12
  cfg$n_cand <- 200

  # Oracle: score candidates by the negated true objective (greedy descent),
  # so this exercises the categorical decoding through run_bo without a surrogate.
  oracle <- list(
    name       = "oracle",
    candidates = function(X_eval, y_eval)
      space_filling_candidates(cfg$n_cand, obj$d),
    acquire    = function(X_eval, y_eval, X_cand) -obj$fn(X_cand)
  )

  X_init <- space_filling_candidates(2 * obj$d + 1, obj$d)
  y_init <- obj$fn(X_init)
  best   <- run_bo(obj, oracle, cfg, X_init, y_init)$best

  expect_length(best, cfg$budget + 1)
  expect_true(all(diff(best) <= 0))        # best-so-far never worsens
  expect_lt(best[length(best)], best[1])   # and improves
})
