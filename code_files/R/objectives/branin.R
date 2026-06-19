# =========================
# Branin function
# =========================

branin <- function(xx,
                   a = 1,
                   b = 5.1 / (4 * pi^2),
                   c = 5 / pi,
                   r = 6,
                   s = 10,
                   t = 1 / (8 * pi)) {
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- a * (x2 - b * x1^2 + c * x1 - r)^2
  term2 <- s * (1 - t) * cos(x1)
  
  term1 + term2 + s
}

# =========================
# Domain (IMPORTANT)
# =========================

branin_bounds <- list(
  lower = c(-5, 0),
  upper = c(10, 15)
)
