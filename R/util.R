#' @importFrom graphics par
#' @importFrom methods is
#' @importFrom stats dbeta dnorm qgamma quantile rbeta rgamma rnorm dgamma dpois qpois runif

getProb <- function(A_samples, B_samples, percent_lift = 0) {
  mean((100 * (A_samples - B_samples) / B_samples > percent_lift))
}

getCredInt <- function(A_samples, B_samples, prop = .9) {
  diff <- (A_samples - B_samples) / B_samples
  crit <- (1 - prop) / 2
  quantile(diff, c(crit, 1 - crit))
}

dpareto <- function(x, xm, alpha) ifelse(x > xm , alpha * xm ** alpha / (x ** (alpha + 1)), 0)
ppareto <- function(q, xm, alpha) ifelse(q > xm , 1 - (xm / q) ** alpha, 0 )
qpareto <- function(p, xm, alpha) ifelse(p < 0 | p > 1, NaN, xm * (1 - p) ** (-1 / alpha))
rpareto <- function(n, xm, alpha) qpareto(runif(n), xm, alpha)
