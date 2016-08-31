#' @importFrom graphics par
#' @importFrom methods is
#' @importFrom stats dbeta dnorm qgamma quantile rbeta rgamma rnorm

getProb <- function(A_samples, B_samples, percent_lift = 0) {
  mean((100 * (A_samples - B_samples) / B_samples > percent_lift))
}

getCredInt <- function(A_samples, B_samples, prop = .9) {
  diff <- (A_samples - B_samples) / B_samples
  crit <- (1 - prop) / 2
  quantile(diff, c(crit, 1 - crit))
}
