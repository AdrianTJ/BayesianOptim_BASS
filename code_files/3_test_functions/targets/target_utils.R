# =========================
# Utilities for targets
# =========================

scale01_to_bounds <- function(X, lower, upper) {
  X <- as.matrix(X)
  sweep(X, 2, lower, `+`) +
    sweep(X, 2, upper - lower, `*`)
}

vectorize_target <- function(fn, bounds) {
  force(fn)
  force(bounds)
  
  function(X) {
    X <- as.matrix(X)
    if (is.null(nrow(X))) X <- matrix(X, nrow = 1)
    
    X_phys <- scale01_to_bounds(X, bounds$lower, bounds$upper)
    apply(X_phys, 1, fn)
  }
}
