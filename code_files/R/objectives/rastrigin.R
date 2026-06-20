#' Rastrigin Benchmark Function
#'
#' A non-convex function used as a performance test problem for optimization algorithms.
#' It is a typical example of a non-linear multimodal function. It was first proposed
#' by Rastrigin as a 2-dimensional function and has been generalized by Rudolph.
#'
#' Features many local minima, but the global minimum is at x = (0, ..., 0) where f(x) = 0.
#'
#' @param xx A numeric vector of length d.
#' @return The function value at xx.
rastrigin <- function(xx)
{
  d <- length(xx)
  sum_part <- sum(xx^2 - 10 * cos(2 * pi * xx))
  y <- 10 * d + sum_part
  return(y)
}

# ==============================================================================
# Domain Configuration
# ==============================================================================
#' Rastrigin Domain Bounds
#' Usually evaluated on the hypercube x_i in [-5.12, 5.12]
rastrigin_bounds <- list(
  lower = c(-5.12, -5.12, -5.12, -5.12),
  upper = c(5.12, 5.12, 5.12, 5.12)
)

