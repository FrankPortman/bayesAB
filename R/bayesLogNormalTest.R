bayesLogNormalTest <- function(A_data,
                               B_data,
                               m0,
                               k0,
                               s_sq0,
                               v0,
                               n_samples = 1e5) {
  
  A_data <- log(A_data)
  B_data <- log(B_data)
  
  A <- draw_mus_and_sigmas(A_data, m0, k0, s_sq0, v0, n_samples)
  B <- draw_mus_and_sigmas(B_data, m0, k0, s_sq0, v0, n_samples)
  
  ## Means
  A_mus <- A$mu_samples
  B_mus <- B$mu_samples
  
  ## Sigmas
  A_sig_sqs <- A$sig_sq_samples
  B_sig_sqs <- B$sig_sq_samples
  
  ## Transform back to log normal for interpretation
  A_means <- exp(A_mus + A_sig_sqs / 2)
  B_means <- exp(B_mus + B_sig_sqs / 2)
  
  A_meds <- exp(A_mus)
  B_meds <- exp(B_mus)
  
  A_modes <- exp(A_mus - A_sig_sqs)
  B_modes <- exp(B_mus - B_sig_sqs)
  
  A_vars <- (exp(A_sig_sqs) - 1) * exp(2 * A_mus + A_sig_sqs)
  B_vars <- (exp(B_sig_sqs) - 1) * exp(2 * B_mus + B_sig_sqs)
  
  
  result <- list(mean_prob = mean(A_means > B_means),
                 med_prob = mean(A_meds > B_meds),
                 mode_prob = mean(A_modes > B_modes),
                 var_prob = mean(A_vars > B_vars),
                 
                 inputs = list(
                   A_data = A_data,
                   B_data = B_data,
                   m0 = m0,
                   k0 = k0,
                   s_sq0 = s_sq0,
                   v0 = v0,
                   n_samples = n_samples
                 ),
                 
                 posteriors = list(
                   A_mus = A_mus,
                   B_mus = B_mus,
                   
                   A_sig_sqs = A_sig_sqs,
                   B_sig_sqs = B_sig_sqs,
                   
                   A_means = A_means,
                   B_means = B_means,
                   
                   A_meds = A_meds,
                   B_meds = B_meds,
                   
                   A_modes = A_modes,
                   B_modes = B_modes,
                   
                   A_vars = A_vars,
                   B_vars = B_vars,
                   
                   
                   
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