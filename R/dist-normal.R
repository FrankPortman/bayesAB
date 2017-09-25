drawMusAndSigmas <- function(data,
                             mu,
                             lambda,
                             alpha,
                             beta,
                             n_samples) {

  n <- length(data)
  xbar <- mean(data)

  ss <- var(data) * (n - 1)

  new_mu <- lambda * mu + n * xbar
  new_mu <- new_mu / (lambda + n)

  new_lambda <- lambda + n

  new_alpha <- alpha + n / 2

  new_beta <- beta + .5 * (ss + (n * lambda) / (lambda + n) * (xbar - mu) ^ 2)

  rNormalInverseGamma(n_samples, new_mu, new_lambda, new_alpha, new_beta)

}
