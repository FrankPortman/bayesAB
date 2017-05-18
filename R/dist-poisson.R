bayesPoissonTest <- function(A_data,
                             B_data,
                             priors,
                             n_samples) {

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

  ## Check that priors are supplied
  if(length(priors) != 2) stop("Incorrect length of priors. Expecting an argument for shape and rate ONLY.")

  ## Check we have alpha and beta
  if(!all(names(priors) %in% c('shape', 'rate'))) stop("Arguments don't match requirement for shape and rate. Check names.")

  priors <- priors[c('shape', 'rate')]
  priors <- suppressWarnings(as.numeric(priors))

  if(any(is.na(priors))) stop("shape and/or rate are not numeric!")
  if(!all(priors > 0)) stop("shape and rate are parameters of the Gamma Distribution and should be strictly > 0.")

  shape <- priors[1]
  rate <- priors[2]

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
