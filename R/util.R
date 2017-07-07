getProb <- function(vals, percent_lift = 0) {
  mean((100 * vals > percent_lift))
}

getCredInt <- function(vals, prop = .9) {
  crit <- (1 - prop) / 2
  quantile(vals, c(crit, 1 - crit))
}

getLift <- function(A_samples, B_samples) {
  (A_samples - B_samples) / B_samples
}

coalesce <- function(n) ifelse(is.na(n) | is.nan(n), 0, n)

getPostError <- function(A_samples, B_samples) {
  #expected loss from switching from B to A
  coalesce(mean(B_samples > A_samples) * mean(B_samples[B_samples > A_samples]))
}

listOr <- function(e) if(is.list(e)) e else list(e)

isClosed <- function(distribution) grepl("C", distribution)
