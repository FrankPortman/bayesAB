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

# Expected loss from switching from B to A
getPostError <- function(A_samples, B_samples) {
  BoverA <- B_samples > A_samples
  coalesce(mean(BoverA) * mean(B_samples[BoverA]))
}

isClosed <- function(distribution) grepl("C", distribution)

removeGenericArgs <- function(vec) {
  vec[!vec %in% c('A_data', 'B_data', 'n_samples')]
}

funcLooper <- function(inputs, funcs) {
  for(func in funcs) do.call(func, inputs)
}
