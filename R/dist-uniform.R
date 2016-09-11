bayesUniformTest <- function(A_data,
                             B_data,
                             priors,
                             n_samples = 1e6) {
  
  ###
  ## Error Checking
  ###
  
  ## Check that we only have positive data
  if((
    any(
      A_data <= 0,
      B_data <= 0
    )
  )) {
    stop("Data input is incorrect. The support of a Uniform distribution in bayesAB is (0, Inf).")
  }
  
  ## Check that priors are supplied
  if(length(priors) != 2) stop("Incorrect length of priors. Expecting an argument for xm and alpha ONLY.")
  
  ## Check we have alpha and beta
  if(!all(names(priors) %in% c('xm', 'alpha'))) stop("Arguments don't match requirement for xm and alpha. Check names.")
  
  priors <- priors[c('xm', 'alpha')]
  priors <- suppressWarnings(as.numeric(priors))
  
  if(any(is.na(priors))) stop("xm and/or alpha are not numeric!")
  if(!all(priors > 0)) stop("xm and alpha are parameters of the Pareto Distribution and should be strictly > 0.")
  
  xm <- priors[1]
  alpha <- priors[2]
  
  ###
  ## Sample from posterior
  ###
  
  A_thetas <- rpareto(n_samples, max(A_data, xm), length(A_data) + alpha)
  B_thetas <- rpareto(n_samples, max(B_data, xm), length(B_data) + alpha)
  
  ###
  ## Output the result
  ###
  
  result <- list(
    inputs = as.list(match.call()[-1]),
    
    posteriors = list(
      Theta = list(A_thetas = A_thetas, B_thetas = B_thetas)
    ),
    
    distribution = "uniform"
  )
  
  class(result) <- c('bayesTest')
  
  return(result)
  
}
