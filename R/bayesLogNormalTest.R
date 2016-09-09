bayesLogNormalTest <- function(A_data,
                               B_data,
                               priors,
                               n_samples = 1e5) {
  
  ###
  ## Error Checking
  ###
  
  if((
    any(
      A_data <= 0,
      B_data <= 0
    )
  )) {
    stop("Data input is incorrect. The support of a Log Normal Distribution is (0, Inf).")
  }
  
  if(any(is.na(suppressWarnings(as.numeric(c(A_data, B_data)))))) stop("A_data and B_data are not ALL numeric.")
  
  ###
  ## Sample from posterior
  ###
  
  NormalResult <- bayesNormalTest(log(A_data), log(B_data), priors, n_samples)
  
  ## Means
  A_mus <- NormalResult$posteriors$Mu$A_mus
  B_mus <- NormalResult$posteriors$Mu$B_mus
  
  ## Sigmas
  A_sig_sqs <- NormalResult$posteriors$Sig_Sq$A_sig_sqs
  B_sig_sqs <- NormalResult$posteriors$Sig_Sq$B_sig_sqs
  
  ## Transform back to log normal for interpretation
  A_means <- exp(A_mus + A_sig_sqs / 2)
  B_means <- exp(B_mus + B_sig_sqs / 2)
  
  A_vars <- (exp(A_sig_sqs) - 1) * exp(2 * A_mus + A_sig_sqs)
  B_vars <- (exp(B_sig_sqs) - 1) * exp(2 * B_mus + B_sig_sqs)
  
  ###
  ## Output the result
  ###
  
  result <- list(
    inputs = as.list(match.call()[-1]),
    
    posteriors = list(
      Mu = list(A_mus = A_mus, B_mus = B_mus),
      Sig_Sq = list(A_sig_sqs = A_sig_sqs, B_sig_sqs = B_sig_sqs),
      Mean = list(A_means = A_means, B_means = B_means),
      Var = list(A_vars = A_vars, B_vars = B_vars)
    ),
    
    distribution = "lognormal"
  )
  
  class(result) <- c('bayesTest')
  
  return(result)
  
}
