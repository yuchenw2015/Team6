helper <- function(y) {
  data <- data.frame(long = y[, 1],
                     lat  = y[, 2],
                     groups = rep(rnorm(1),nrow(y)),
                     order = 1:nrow(y))
  data
}
