bayesUniformTest <- function(A_data,
                          B_data,
                          priors,
                          N_samp = 1e6) {
  
  ###
  ## Error Checking
  ###
  
  
  ## Check that we only have integer data
  
  ## Check that priors are supplied
  if(length(priors) != 2) stop("Incorrect length of priors. Expecting an argument for shape and rate ONLY.")
  
  ## Check we have alpha and beta
  if(!all(names(priors) %in% c('xm', 'alpha'))) stop("Arguments don't match requirement for xm and alpha. Check names.")
  
  priors <- priors[c('xm', 'alpha')]
  stored_priors <- priors
  priors <- suppressWarnings(as.numeric(priors))
    
  if(any(is.na(priors))) stop("xm and/or alpha are not numeric!")
  if(!all(priors > 0)) stop("xm and alpha are parameters of the Pareto Distribution and should be strictly > 0.")
  
  xm <- priors[1]
  alpha <- priors[2]
    
  ###
  ## Do the computation
  ###
    
  clicks_A <- sum(A_data)
  views_A <- length(A_data)
  
  clicks_B <- sum(B_data)
  views_B <- length(B_data)
  
  A_thetas <- rgamma(N_samp, max(A_data, xm), length(A_data) + alpha)
  B_thetas <- rgamma(N_samp, max(B_data, xm), length(B_data) + alpha)
  
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
      Theta = list(A_thetas = A_thetas, B_thetas = B_thetas)
    ),
    
    distribution = "uniform"
  )
  
  class(result) <- c('bayesTest')
  
  return(result)
  
}
