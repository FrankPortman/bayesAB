drawMusAndSigmas <- function(data,
                             mu_0,
                             nu_0,
                             alpha_0,
                             beta_0,
                             n_samples) {

  N <- length(data)
  the_mean <- mean(data)

  ssd <- sum((data - the_mean) ^ 2)

  nu_n <- nu_0 + N
  mu_n <- (nu_0 * mu_0 + N * the_mean) / nu_n

  alpha_n <- alpha_0 + N / 2
  beta_n <- beta_0 + ssd / 2 + N * nu_0 * (the_mean - mu_0)^2 / nu_n / 2

  sig_sq_samples <- (1 / rgamma(n_samples, alpha_n, beta_n))

  mean_norm <- mu_n
  sd_norm <- sqrt(sig_sq_samples / nu_0)
  mu_samples <- rnorm(n_samples, mean_norm, sd_norm)

  return(list(mu_samples = mu_samples, sig_sq_samples = sig_sq_samples))

}
