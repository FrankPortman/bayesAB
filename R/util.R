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

listConcat <- function(l1, l2) {

  out <- vector(mode = "list", length = length(l1) + length(l2))

  for(i in 1:length(l1)) out[[i]] <- l1[[i]]
  for(i in 1:length(l2)) out[[i + length(l1)]] <- l2[[i]]

  out

}

listOr <- function(e) if(is.list(e)) e else list(e)

isClosed <- function(distribution) grepl("C", distribution)
