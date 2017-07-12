bayesPoissonTest <- function(A_data,
                             B_data,
                             n_samples,
                             shape,
                             rate) {

  ###
  ## Error Checking
  ###

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

  ###
  ## Sample from posterior
  ###

  map <- function(data) rgamma(n_samples, sum(data) + shape, length(data) + rate)

  ###
  ## Output the result
  ###

  result <- list(
    inputs = as.list(match.call()[-1]),

    posteriors = list(
      Lambda = list(A = map(A_data), B = map(B_data))
    ),

    distribution = "poisson"
  )

  class(result) <- c('bayesTest')

  return(result)

}
