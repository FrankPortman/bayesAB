bayesExponentialTest <- function(A_data,
                                 B_data,
                                 n_samples,
                                 shape,
                                 rate) {

  ###
  ## Error Checking
  ###

  ## Check that we only have positive data
  if((
    any(
      A_data < 0,
      B_data < 0
    )
  )) {
    stop("Data input is incorrect. The support of an Exponential Distribution is [0, Inf).")
  }

  if(!all(c(shape, rate) > 0)) stop("shape and rate are parameters of the Gamma Distribution and should be strictly > 0.")

  ###
  ## Sample from posterior
  ###

  map <- function(data) rgamma(n_samples, length(data) + shape, sum(data) + rate)

  ###
  ## Output the result
  ###

  result <- list(
    inputs = as.list(match.call()[-1]),

    posteriors = list(
      Lambda = list(A = map(A_data), B = map(B_data))
    ),

    distribution = "exponential"
  )

  class(result) <- c('bayesTest')

  return(result)

}
