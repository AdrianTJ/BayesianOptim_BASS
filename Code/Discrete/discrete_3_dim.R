library(BASS)
library(lhs)

f <- function(x) {
  ifelse(x[, 3] == "A",
         10 * sin(pi * x[, 1] * x[, 2]) + 
         10 * (x[, 1] - 0.5)^2 + 
         10 * x[, 2] + 5 * x[, 1],
         20 * sin(pi * x[, 1] * x[, 2]) + 
         20 * (x[, 1] - 0.5)^2 + 
         20 * x[, 2] + 5 * x[, 1])
}

sigma <- 1  # Noise standard deviation

n0 <- 10
dims <- 2
x <- matrix(runif(n0 * dims), n0, dims)
x_df <- data.frame(x)
x_df[, 3] <- factor(sample(c("A", "B", "C"), n0, replace = TRUE))
y <- rnorm(n0, f(x_df), sigma)

fit <- bass(x_df, y, verbose = FALSE)

x_new <- maximinLHS(10,dims)
x_new_df <- data.frame(x_new)
x_new_df[, 3] <- factor(sample(c("A", "B", "C"), 10, replace = TRUE))

################################################################################
# Problem here. Predict new for cats is not working?
pred <- predict(fit, newdata = data.frame(x = x_new_df))
################################################################################

mu <- apply(pred, 2, mean)
