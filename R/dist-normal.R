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
