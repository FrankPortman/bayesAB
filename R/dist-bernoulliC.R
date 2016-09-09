bayesBernoulliTestClosed <- function(A_data,
                                     B_data,
                                     priors) {
  
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
  priors <- suppressWarnings(as.numeric(priors))
  
  if(any(is.na(priors))) stop("alpha and/or beta are not numeric!")
  if(!all(priors > 0)) stop("alpha and beta are parameters of the Beta Distribution and should be strictly > 0.")
  
  alpha <- priors[1]
  beta <- priors[2]
  
  ###
  ## Do the computation
  ###
  
  prob <- bayesBernoulliTestClosed_(sum(A_data) + alpha, 
                                    length(A_data) - sum(A_data) + beta,
                                    sum(B_data) + alpha,
                                    length(B_data) - sum(B_data) + beta)
  
  ###
  ## Output the result
  ###
  
  result <- list(
    inputs = as.list(match.call()[-1]),
    
    posteriors = list(Probability = prob),
    
    distribution = 'bernoulliC'
  )
  
  class(result) <- 'bayesTestClosed'
  
  return(result)
  
}
