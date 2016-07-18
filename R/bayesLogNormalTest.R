bayesLogNormalTest <- function(A_data,
                               B_data,
                               m0,
                               k0,
                               s_sq0,
                               v0,
                               percent_lift = 0,
                               n_samples = 1e5) {
  
  if(!(
    any(
      A_data <= 0,
      B_data <= 0
      )
    )) {
    stop("Data input is incorrect. The support of a Log Normal Distribution is (0, Inf).")
  }
  
  A_data <- log(A_data)
  B_data <- log(B_data)
  
  NormalResult <- bayesNormalTest(A_data, B_data, m0, k0, s_sq0, v0, percent_lift, n_samples)
  
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
  
  
  result <- list(mean_prob = getProb(A_means, B_means, percent_lift),
                 med_prob = getProb(A_meds, B_meds, percent_lift),
                 mode_prob = getProb(A_modes, B_modes, percent_lift),
                 var_prob = getProb(A_vars, B_vars, perecnt_lift),
                 
                 inputs = list(
                   A_data = A_data,
                   B_data = B_data,
                   m0 = m0,
                   k0 = k0,
                   s_sq0 = s_sq0,
                   v0 = v0,
                   percent_lift = percent_lift,
                   n_samples = n_samples
                 ),
                 
                 posteriors = list(
                   A_mus = A_mus,
                   B_mus = B_mus,
                   
                   A_sig_sqs = A_sig_sqs,
                   B_sig_sqs = B_sig_sqs,
                   
                   statistics = list(
                     
                     A_means = A_means,
                     B_means = B_means,
                     
                     A_meds = A_meds,
                     B_meds = B_meds,
                     
                     A_modes = A_modes,
                     B_modes = B_modes,
                     
                     A_vars = A_vars,
                     B_vars = B_vars
                     
                   ),
                   
                   alphas = list(
                     A_alpha = A$alpha,
                     B_alpha = B$alpha
                   ),
                   betas = list(
                     A_beta = A$beta,
                     B_beta = B$beta
                   )
                 )
                 
                 
  )
  
  class(result) <- c('bayesLogNormalTest','bayesTest')
  
  return(result)
  
}