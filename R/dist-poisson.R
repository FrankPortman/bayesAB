poissonChecks <- function(A_data, B_data, shape, rate, ...) {
  if((
    any(
      A_data < 0,
      B_data < 0,
      as.integer(A_data) != A_data,
      as.integer(B_data) != B_data
    )
  )) {
    stop("Data input is incorrect. The support of a Poisson distribution is Z*.")
  }
  
  if(!all(c(shape, rate) > 0)) stop("shape and rate are parameters of the Gamma Distribution and should be strictly > 0.")
}

bayesPoissonTest <- function(A_data,
                             B_data,
                             n_samples,
                             shape,
                             rate) {
  do.call(poissonChecks, as.list(match.call()[-1]))
  map <- function(data) rgamma(n_samples, sum(data) + shape, length(data) + rate)
  list(
    Lambda = list(A = map(A_data), B = map(B_data))
  )
}

bayesPoissonTestClosed <- function(A_data,
                                   B_data,
                                   shape,
                                   rate) {
  do.call(poissonChecks, as.list(match.call()[-1]))
  prob <- bayesPoissonTestClosed_(sum(A_data) + shape,
                                  length(A_data) + rate,
                                  sum(B_data) + shape,
                                  length(B_data) + rate)
  list(Lambda = prob)
}
