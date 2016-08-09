drawMusAndSigmas <- function(data, 
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
                            priors,
                            percent_lift = 0,
                            n_samples = 1e5) {
  
  ###
  ## Error Checking
  ###
  
  ## Check that we have 4 priors
  if(length(priors) != 4) stop("Incorrect length of priors. Expecting an argument for m0, k0, s_sq0, and v0.")
  
  ## Check we have all priors
  if(!all(names(priors) %in% c('m0', 'k0', 's_sq0', 'v0'))) 
    stop("Arguments don't match requirement for m0, k0, s_sq0, and v0. Check names.")
  
  
  priors <- priors[c('m0', 'k0', 's_sq0', 'v0')]
  stored_priors <- priors
  priors <- as.numeric(priors)
  
  m0 <- priors[1]
  k0 <- priors[2]
  s_sq0 <- priors[3]
  v0 <- priors[4]
  
  ## Check that priors are numeric
  if(any(is.na(priors))) stop("One or more of the priors is not numeric.")
  
  if(k0 <= 0) stop('k0 is the "variance" prior on mu ~ N(m0, k0) and must be strictly positive.')
  if(s_sq0 <= 0) stop('s_sq0 is the "alpha" prior on sig_sq ~ InvGamma(s_sq0, v0) and must be strictly positive.')
  if(v0 <= 0) stop('v0 is the "beta" prior on sig_sq ~ InvGamma(s_sq0, v0) and must be strictly positive.')
  
  ###
  ## Do the computation
  ###
  
  A <- drawMusAndSigmas(A_data, m0, k0, s_sq0, v0, n_samples)
  B <- drawMusAndSigmas(B_data, m0, k0, s_sq0, v0, n_samples)
  
  A_mus <- A$mu_samples
  B_mus <- B$mu_samples
  
  A_sig_sqs <- A$sig_sq_samples
  B_sig_sqs <- B$sig_sq_samples
  
  ###
  ##  Output the result
  ###
  
  result <- list(mu_prob = getProb(A_mus, B_mus, percent_lift = percent_lift),
                 sig_sq_prob = getProb(A_sig_sqs, B_sig_sqs, percent_lift = percent_lift),
                 
                 inputs = list(
                   A_data = A_data,
                   B_data = B_data,
                   priors = stored_priors,
                   percent_lift = percent_lift,
                   n_samples = n_samples
                 ),
                 
                 trans_inputs = list(
                   alphas = list(
                     A_alpha = A$alpha,
                     B_alpha = B$alpha
                   ),
                   betas = list(
                     A_beta = A$beta,
                     B_beta = B$beta
                   )
                 ),
                 
                 posteriors = list(
                   Mu = list(A_mus = A_mus, B_mus = B_mus),
                   Sig_Sq = list(A_sig_sqs = A_sig_sqs, B_sig_sqs = B_sig_sqs)
                 )
                 
                 
  )
  
  class(result) <- c('bayesNormalTest','bayesTest')
  
  return(result)
  
}
