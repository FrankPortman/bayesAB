bayesBernoulliTest <- function(A_data,
                          B_data,
                          priors,
                          N_samp = 1e6) {
  
  ###
  ## Error Checking
  ###
  
  
  ## Check that we only have click data
  if(!(
    all(
      A_data %in% c(0, 1),
      B_data %in% c(0, 1)
      )
    )) {
    stop("Data input is incorrect. Data can only contain 0's and 1's. See help docs for more info.")
  }
  
  ## Check that priors are supplied
  if(length(priors) != 2) stop("Incorrect length of priors. Expecting an argument for alpha and beta ONLY.")
  
  ## Check we have alpha and beta
  if(!all(names(priors) %in% c('alpha', 'beta'))) stop("Arguments don't match requirement for alpha and beta. Check names.")
  
  priors <- priors[c('alpha', 'beta')]
  stored_priors <- priors
  priors <- suppressWarnings(as.numeric(priors))
    
  if(any(is.na(priors))) stop("alpha and/or beta are not numeric!")
  if(!all(priors > 0)) stop("alpha and beta are parameters of the Beta Distribution and should be strictly > 0.")
  
  alpha <- priors[1]
  beta <- priors[2]
    
  ###
  ## Do the computation
  ###
    
  clicks_A <- sum(A_data)
  views_A <- length(A_data)
  
  clicks_B <- sum(B_data)
  views_B <- length(B_data)
  
  A_probs <- rbeta(N_samp, clicks_A + alpha, views_A - clicks_A + beta)
  B_probs <- rbeta(N_samp, clicks_B + alpha, views_B - clicks_B + beta)
  
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
      Probability = list(A_probs = A_probs, B_probs = B_probs)
    ),
    
    distribution = "bernoulli"
  )
  
  class(result) <- c('bayesTest')
  
  return(result)
  
}
