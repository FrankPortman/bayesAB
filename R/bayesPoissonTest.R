bayesPoissonTest <- function(A_data,
                          B_data,
                          priors,
                          N_samp = 1e6) {
  
  ###
  ## Error Checking
  ###
  
  
  ## Check that we only have click data
  
  ## Check that priors are supplied
  if(length(priors) != 2) stop("Incorrect length of priors. Expecting an argument for shape and rate ONLY.")
  
  ## Check we have alpha and beta
  if(!all(names(priors) %in% c('shape', 'rate'))) stop("Arguments don't match requirement for shape and rate. Check names.")
  
  priors <- priors[c('shape', 'rate')]
  stored_priors <- priors
  priors <- suppressWarnings(as.numeric(priors))
    
  if(any(is.na(priors))) stop("shape and/or rate are not numeric!")
  if(!all(priors > 0)) stop("shape and rate are parameters of the Gamma Distribution and should be strictly > 0.")
  
  shape <- priors[1]
  rate <- priors[2]
    
  ###
  ## Do the computation
  ###
    
  clicks_A <- sum(A_data)
  views_A <- length(A_data)
  
  clicks_B <- sum(B_data)
  views_B <- length(B_data)
  
  A_lambdas <- rgamma(N_samp, sum(A_data) + shape, length(A_data) + rate)
  B_lambdas <- rgamma(N_samp, sum(B_data) + shape, length(B_data) + rate)
  
  ###
  ## Output the result
  ###
  
  result <- list(
    
    inputs = list(
      A_data = A_data,
      B_data = B_data,
      priors = stored_priors,
      n_samples = N_samp
    ),
    
    posteriors = list(
      Lambda = list(A_lambdas = A_lambdas, B_lambdas = B_lambdas)
    ),
    
    distribution = "poisson"
  )
  
  class(result) <- c('bayesTest')
  
  return(result)
  
}
