drawMusAndSigmas <- function(data,
                             m0,
                             k0,
                             s_sq0,
                             v0,
                             n_samples) {

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

  return(list(mu_samples = mu_samples, sig_sq_samples = sig_sq_samples))

}

bayesNormalTest <- function(A_data,
                            B_data,
                            n_samples,
                            m0,
                            k0,
                            s_sq0,
                            v0) {

  if(k0 <= 0) stop("k0 is the 'variance' prior on mu ~ N(m0, k0) and must be strictly positive.")
  if(s_sq0 <= 0) stop("s_sq0 is the 'alpha' prior on sig_sq ~ InvGamma(s_sq0, v0) and must be strictly positive.")
  if(v0 <= 0) stop("v0 is the 'beta' prior on sig_sq ~ InvGamma(s_sq0, v0) and must be strictly positive.")

  A <- drawMusAndSigmas(A_data, m0, k0, s_sq0, v0, n_samples)
  B <- drawMusAndSigmas(B_data, m0, k0, s_sq0, v0, n_samples)

  A_mus <- A$mu_samples
  B_mus <- B$mu_samples

  A_sig_sqs <- A$sig_sq_samples
  B_sig_sqs <- B$sig_sq_samples

  list(
    Mu = list(A = A_mus, B = B_mus),
    Sig_Sq = list(A = A_sig_sqs, B = B_sig_sqs)
  )
}
