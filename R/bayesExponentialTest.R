bayesExponentialTest <- function(A_data,
                                 B_data,
                                 priors,
                                 n_samples = 1e6) {
  
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
  
  A_lambdas <- rgamma(n_samples, length(A_data) + shape, sum(A_data) + rate)
  B_lambdas <- rgamma(n_samples, length(B_data) + shape, sum(B_data) + rate)
  
  ###
  ## Output the result
  ###
  
  result <- list(
    inputs = as.list(match.call()[-1]),
    
    posteriors = list(
      Lambda = list(A_lambdas = A_lambdas, B_lambdas = B_lambdas)
    ),
    
    distribution = "exponential"
  )
  
  class(result) <- c('bayesTest')
  
  return(result)
  
}
