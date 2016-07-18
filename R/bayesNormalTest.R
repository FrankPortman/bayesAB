draw_mus_and_sigmas <- function(data, 
                                m0, 
                                k0, 
                                s_sq0, 
                                v0, 
                                n_samples = 1e5) {
  
  N <- length(data)
  the_mean <- mean(data)
  
  ssd <- sum((data - the_mean) ^ 2)
  
  kN <- k0 + N
  mN <- (k0 / kN) * m0 + (N / kN) * the_mean
  vN <- v0 + N
  
  vN_times_s_sqN <- v0 * s_sq0 + ssd + (N * k0 * (m0 - the_mean) ^ 2) / kN
  
  
  alpha <- vN / 2
  beta <- vN_times_s_sqN / 2
  
  sig_sq_samples <- (1 / rgamma(n_samples, alpha, beta))
  
  mean_norm <- mN
  var_norm <- sqrt(sig_sq_samples / kN)
  mu_samples <- rnorm(n_samples, mean_norm, var_norm)
  
  return(list(mu_samples = mu_samples,
              sig_sq_samples = sig_sq_samples,
              alpha = alpha,
              beta = beta))
  
}

bayesNormalTest <- function(A_data,
                            B_data,
                            m0,
                            k0,
                            s_sq0,
                            v0,
                            percent_lift = 0,
                            n_samples = 1e5) {
  
  
  A <- draw_mus_and_sigmas(A_data, m0, k0, s_sq0, v0, n_samples)
  B <- draw_mus_and_sigmas(B_data, m0, k0, s_sq0, v0, n_samples)
  
  A_mus <- A$mu_samples
  B_mus <- B$mu_samples
  
  A_sig_sqs <- A$sig_sq_samples
  B_sig_sqs <- B$sig_sq_samples
  
  result <- list(mu_prob = getProb(A_mus, B_mus, percent_lift = percent_lift),
                 sig_sq_prob = getProb(A_sig_sqs, B_sig_sqs, percent_lift = percent_lift),
                 
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
  
  class(result) <- c('bayesNormalTest','bayesTest')
  
  return(result)
  
}
