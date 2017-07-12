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
                            mu,
                            sd,
                            shape,
                            scale) {

  if(sd <= 0) stop("sd is the 'sd' prior on Mu ~ N(mu, sd^2) and must be strictly positive.")
  if(shape <= 0) stop("shape is the 'shape' prior on sig_sq ~ InvGamma(shape, scale) and must be strictly positive.")
  if(scale <= 0) stop("scale is the 'scale' prior on sig_sq ~ InvGamma(shape, scale) and must be strictly positive.")

  A <- drawMusAndSigmas(A_data, mu, sd, shape, scale, n_samples)
  B <- drawMusAndSigmas(B_data, mu, sd, shape, scale, n_samples)

  A_mus <- A$mu_samples
  B_mus <- B$mu_samples

  A_sig_sqs <- A$sig_sq_samples
  B_sig_sqs <- B$sig_sq_samples

  list(
    Mu = list(A = A_mus, B = B_mus),
    Sig_Sq = list(A = A_sig_sqs, B = B_sig_sqs)
  )
}
