bayesLogNormalTest <- function(A_data,
                               B_data,
                               priors,
                               percent_lift = 0,
                               n_samples = 1e5) {
  
  if((
    any(
      A_data <= 0,
      B_data <= 0
    )
  )) {
    stop("Data input is incorrect. The support of a Log Normal Distribution is (0, Inf).")
  }
  
  A_data <- log(A_data)
  B_data <- log(B_data)
  
  NormalResult <- bayesNormalTest(A_data, B_data, priors, percent_lift, n_samples)
  
  ## Means
  A_mus <- NormalResult$posteriors$A_mus
  B_mus <- NormalResult$posteriors$B_mus
  
  ## Sigmas
  A_sig_sqs <- NormalResult$posteriors$A_sig_sqs
  B_sig_sqs <- NormalResult$posteriors$B_sig_sqs
  
  ## Transform back to log normal for interpretation
  A_means <- exp(A_mus + A_sig_sqs / 2)
  B_means <- exp(B_mus + B_sig_sqs / 2)
  
  A_meds <- exp(A_mus)
  B_meds <- exp(B_mus)
  
  A_modes <- exp(A_mus - A_sig_sqs)
  B_modes <- exp(B_mus - B_sig_sqs)
  
  A_vars <- (exp(A_sig_sqs) - 1) * exp(2 * A_mus + A_sig_sqs)
  B_vars <- (exp(B_sig_sqs) - 1) * exp(2 * B_mus + B_sig_sqs)
  
  #transform data to original form before returning it:
  A_data <- exp(A_data)
  B_data <- exp(B_data)
  
  result <- list(mean_prob = getProb(A_means, B_means, percent_lift),
                 med_prob = getProb(A_meds, B_meds, percent_lift),
                 mode_prob = getProb(A_modes, B_modes, percent_lift),
                 var_prob = getProb(A_vars, B_vars, percent_lift),
                 
                 inputs = list(
                   A_data = A_data,
                   B_data = B_data,
                   priors = priors,
                   percent_lift = percent_lift,
                   n_samples = n_samples
                 ),
                 
                 trans_inputs = list(
                   alphas = list(
                     A_alpha = NormalResult$trans_inputs$alphas$A_alpha,
                     B_alpha = NormalResult$trans_inputs$alphas$B_alpha
                   ),
                   betas = list(
                     A_beta = NormalResult$trans_inputs$betas$A_beta,
                     B_beta = NormalResult$trans_inputs$betas$B_beta
                   )
                 ),
                 
                 posteriors = list(
                   Mu = list(A_mus = A_mus, B_mus = B_mus),
                   Sig_Sq = list(A_sig_sqs = A_sig_sqs, B_sig_sqs = B_sig_sqs),
                   Mean = list(A_means = A_means, B_means = B_means),
                   Median = list(A_meds = A_meds, B_meds = B_meds),
                   Mode = list(A_modes = A_modes, B_modes = B_modes),
                   Var = list(A_vars = A_vars, B_vars = B_vars)
                 )
                 
                 
  )
  
  class(result) <- c('bayesLogNormalTest','bayesTest')
  
  return(result)
  
}