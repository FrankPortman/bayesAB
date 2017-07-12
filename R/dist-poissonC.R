bayesPoissonTestClosed <- function(A_data,
                                   B_data,
                                   shape,
                                   rate) {

  ## Check that we only have integer data
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

  prob <- bayesPoissonTestClosed_(sum(A_data) + shape,
                                  length(A_data) + rate,
                                  sum(B_data) + shape,
                                  length(B_data) + rate)

  list(Lambda = prob)
}
