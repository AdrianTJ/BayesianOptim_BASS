# =============================================================================
# categorical.R  --  Categorical / mixed-variable benchmark objectives
# =============================================================================
# These objectives exist to exercise the one thing a plain Gaussian-process
# surrogate (like GPfit) cannot do natively: optimise over *categorical* inputs,
# whose levels have no meaningful order or distance. BASS handles them directly,
# because its basis can split on subsets of a factor's levels (the Friedman 1991
# MARS / Denison-Mallick-Smith 1998 BMARS treatment of categorical predictors).
#
# As with the continuous benchmarks, every objective is a function of a matrix in
# the unit cube [0, 1]^d and is MINIMISED. Categorical coordinates are decoded to
# 1..L levels via decode_levels(); each objective also advertises a `schema`
# (see objective_utils.R) so the BASS surrogate can hand BASS true factors.
#
# References:
#   * Func-2C / Func-3C: Ru, Alvi, Nguyen, Osborne & Roberts (2020), "Bayesian
#     Optimisation over Multiple Continuous and Categorical Inputs" (CoCaBO),
#     ICML; arXiv:1906.08878. Reproduced from the authors' reference code
#     (github.com/rubinxin/CoCaBO_code, testFunctions/syntheticFunctions.py).
#   * Component functions Rosenbrock, Six-Hump Camel, Beale: Surjanovic & Bingham,
#     Virtual Library of Simulation Experiments (www.sfu.ca/~ssurjano).
#   * Permuted-categorical encoding of a continuous benchmark (Cat-Ackley):
#     the standard way categorical-BO papers build factor benchmarks with a known
#     optimum, e.g. Oh, Tomczak, Gavves & Welling (2019), "Combinatorial Bayesian
#     Optimization using the Graph Cartesian Product" (COMBO), NeurIPS. Ackley
#     itself: Surjanovic & Bingham (as above).
# =============================================================================

# --- CoCaBO component functions ----------------------------------------------
# Each maps a single 2-D continuous point to a scalar, on the [-2, 2]^2 domain
# that func2C/func3C use after their internal `X <- X * 2` rescaling. The /300,
# /10, /50 divisors are reproduced exactly from the CoCaBO reference code so the
# relative weighting of the three components matches the published benchmark.

.cocabo_rosenbrock <- function(x1, x2) {
  (100 * (x2 - x1^2)^2 + (x1 - 1)^2) / 300
}

.cocabo_sixhumpcamp <- function(x1, x2) {
  term1 <- (4 - 2.1 * x1^2 + (x1^4) / 3) * x1^2
  term2 <- x1 * x2
  term3 <- (-4 + 4 * x2^2) * x2^2
  (term1 + term2 + term3) / 10
}

.cocabo_beale <- function(x1, x2) {
  ((1.5 - x1 + x1 * x2)^2 +
     (2.25 - x1 + x1 * x2^2)^2 +
     (2.625 - x1 + x1 * x2^3)^2) / 50
}

# Apply the i-th component (1 = Rosenbrock, 2 = Six-Hump Camel, 3 = Beale)
# row-wise. `which_fn` is a per-row integer vector in 1..3.
.cocabo_apply <- function(which_fn, x1, x2) {
  fns <- list(.cocabo_rosenbrock, .cocabo_sixhumpcamp, .cocabo_beale)
  vapply(seq_along(which_fn),
         function(r) fns[[which_fn[r]]](x1[r], x2[r]),
         numeric(1))
}

#' Func-2C: 2 categorical + 2 continuous CoCaBO benchmark (minimised).
#'
#' Dimensions (in [0, 1]^4): h1 (3 levels), h2 (5 levels), x1, x2 (continuous,
#' mapped to [-1, 1] then to [-2, 2] as in CoCaBO). The two categoricals each pick
#' a component function whose values are summed. The tiny 1e-6 noise term of the
#' original is dropped so the objective is deterministic and reproducible.
#'
#' @param X n x 4 matrix in [0, 1]^4 (or a single point).
#' @return  Numeric vector to be minimised.
func2C <- function(X) {
  X <- as.matrix(X)
  if (is.null(nrow(X))) X <- matrix(X, nrow = 1)

  h1 <- decode_levels(X[, 1], 3L)   # 1=Rosenbrock, 2=Six-Hump, 3=Beale
  h2 <- decode_levels(X[, 2], 5L)   # 1=Rosenbrock, 2=Six-Hump, 3..5=Beale
  x1 <- 4 * X[, 3] - 2              # [0,1] -> [-1,1] -> [-2,2]
  x2 <- 4 * X[, 4] - 2

  # h2: levels {1,2} pick Rosenbrock/Six-Hump, the rest (3,4,5) pick Beale --
  # mirrors the CoCaBO `else` branch over ht2 in {2,3,4} (0-based).
  h2_fn <- ifelse(h2 == 1L, 1L, ifelse(h2 == 2L, 2L, 3L))

  .cocabo_apply(h1, x1, x2) + .cocabo_apply(h2_fn, x1, x2)
}

#' Func-3C: 3 categorical + 2 continuous CoCaBO benchmark (minimised).
#'
#' As Func-2C, plus h3 (4 levels) adding a third, weighted component:
#' level 1 -> 5 x Six-Hump, level 2 -> 2 x Rosenbrock, levels 3,4 -> (h3-1) x Beale
#' (reproducing the CoCaBO `ht3` term, where ht3 is the 0-based index used as the
#' Beale weight).
#'
#' @param X n x 5 matrix in [0, 1]^5 (or a single point).
#' @return  Numeric vector to be minimised.
func3C <- function(X) {
  X <- as.matrix(X)
  if (is.null(nrow(X))) X <- matrix(X, nrow = 1)

  h1 <- decode_levels(X[, 1], 3L)
  h2 <- decode_levels(X[, 2], 5L)
  h3 <- decode_levels(X[, 3], 4L)
  x1 <- 4 * X[, 4] - 2
  x2 <- 4 * X[, 5] - 2

  h2_fn <- ifelse(h2 == 1L, 1L, ifelse(h2 == 2L, 2L, 3L))
  base  <- .cocabo_apply(h1, x1, x2) + .cocabo_apply(h2_fn, x1, x2)

  # h3 term: 1 -> 5*camel, 2 -> 2*rosenbrock, {3,4} -> (h3-1)*beale.
  extra <- ifelse(
    h3 == 1L, 5 * .cocabo_sixhumpcamp(x1, x2),
    ifelse(h3 == 2L, 2 * .cocabo_rosenbrock(x1, x2),
           (h3 - 1) * .cocabo_beale(x1, x2))
  )
  base + extra
}

# --- Permuted-categorical Ackley ---------------------------------------------

#' The Ackley function (minimised; global minimum 0 at the origin).
#'
#' @param x Numeric vector.
#' @return  Scalar Ackley value.
.ackley <- function(x, a = 20, b = 0.2, cc = 2 * pi) {
  d <- length(x)
  -a * exp(-b * sqrt(mean(x^2))) - exp(mean(cos(cc * x))) + a + exp(1)
}

#' Draw a fixed permutation without disturbing the global RNG stream.
#'
#' The Cat-Ackley level->grid map is randomised once (so the levels carry no
#' usable ordering) but must be identical on every worker and across reps. We
#' therefore seed locally and restore the caller's RNG state afterwards, leaving
#' the BO loop's own set.seed(seed) untouched.
#'
#' @param L   Permutation length.
#' @param key Integer seed for this permutation.
#' @return    An integer permutation of 1..L.
.fixed_perm <- function(L, key) {
  has_old <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_old) old <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(key)
  p <- sample.int(L)
  if (has_old) {
    assign(".Random.seed", old, envir = .GlobalEnv)
  } else {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  p
}

#' Build a permuted-categorical Ackley objective (minimised; global minimum 0).
#'
#' Each of `d` inputs is a categorical with `L` levels. Level k of input j is
#' mapped to grid value `grid[perm_j[k]]`, where `grid` is an evenly spaced set
#' of points on Ackley's domain that includes 0, and `perm_j` is a fixed random
#' permutation. The permutation removes any monotone relationship between a
#' level's index and its grid value: a smooth/ordinal encoding (what a plain GP
#' is forced into) sees a jagged surface, whereas BASS -- splitting on subsets of
#' levels -- can still recover the good levels. The optimum is the level per input
#' whose grid value is 0; there f = 0.
#'
#' @param d    Number of categorical inputs.
#' @param L    Levels per input (odd, so 0 lies on the grid). Default 11.
#' @param seed Base seed for the per-input permutations.
#' @return     A list with `fn`, `schema`, `d`, and `opt_levels` (the minimiser).
make_cat_ackley <- function(d, L = 11L, seed = 1L) {
  if (L %% 2L == 0L) stop("Cat-Ackley needs an odd L so 0 lies on the grid.")
  grid   <- seq(-32.768, 32.768, length.out = L)   # symmetric; centre is exactly 0
  zero_k <- (L + 1L) %/% 2L                         # grid index of the 0 value
  perms  <- lapply(seq_len(d), function(j) .fixed_perm(L, seed * 1000L + j))

  fn <- function(X) {
    X <- as.matrix(X)
    if (is.null(nrow(X))) X <- matrix(X, nrow = 1)
    vapply(seq_len(nrow(X)), function(r) {
      g <- vapply(seq_len(d), function(j) {
        lev <- decode_levels(X[r, j], L)
        grid[perms[[j]][lev]]
      }, numeric(1))
      .ackley(g)
    }, numeric(1))
  }

  # The minimiser: for input j, the level that the permutation maps to grid 0.
  opt_levels <- vapply(perms, function(p) which(p == zero_k), integer(1))

  list(
    fn         = fn,
    schema     = list(types = rep("cat", d), levels = rep(as.integer(L), d)),
    d          = d,
    opt_levels = opt_levels
  )
}
