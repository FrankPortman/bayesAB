drawMusAndSigmas <- function(data,
                             mu,
                             sd,
                             shape,
                             scale,
                             n_samples) {

  rate <- 1 / scale

  N <- length(data)
  the_mean <- mean(data)

  ssd <- sum((data - the_mean) ^ 2)

  kN <- sd + N
  mN <- (sd / kN) * mu + (N / kN) * the_mean
  vN <- rate + N

  vN_times_s_sqN <- rate * shape + ssd + (N * sd * (mu - the_mean) ^ 2) / kN

  alpha <- vN / 2
  beta <- vN_times_s_sqN / 2

  sig_sq_samples <- (1 / rgamma(n_samples, alpha, beta))

  mean_norm <- mN
  var_norm <- sqrt(sig_sq_samples) / kN
  mu_samples <- rnorm(n_samples, mean_norm, var_norm)

  return(list(mu_samples = mu_samples, sig_sq_samples = sig_sq_samples))

}
