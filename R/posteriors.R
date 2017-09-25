Bernoulli <- function(A_data,
                      B_data,
                      n_samples,
                      alpha,
                      beta) {
  map <- function(data) rbeta(n_samples, sum(data) + alpha, length(data) - sum(data) + beta)
  list(
    Probability = list(A = map(A_data), B = map(B_data))
  )
}

BernoulliClosed <- function(A_data,
                            B_data,
                            alpha,
                            beta) {
  prob <- BernoulliClosed_(sum(A_data) + alpha,
                           length(A_data) - sum(A_data) + beta,
                           sum(B_data) + alpha,
                           length(B_data) - sum(B_data) + beta)
  list(Probability = prob)
}

Exponential <- function(A_data,
                        B_data,
                        n_samples,
                        shape,
                        rate) {
  map <- function(data) rgamma(n_samples, length(data) + shape, sum(data) + rate)
  list(
    Lambda = list(A = map(A_data), B = map(B_data))
  )
}

Normal <- function(A_data,
                   B_data,
                   n_samples,
                   mu,
                   lambda,
                   alpha,
                   beta) {
  A <- drawMusAndSigmas(A_data, mu, lambda, alpha, beta, n_samples)
  B <- drawMusAndSigmas(B_data, mu, lambda, alpha, beta, n_samples)

  A_mus <- A$mu
  B_mus <- B$mu

  A_sig_sqs <- A$sig_sq
  B_sig_sqs <- B$sig_sq

  list(
    Mu = list(A = A_mus, B = B_mus),
    Sig_Sq = list(A = A_sig_sqs, B = B_sig_sqs)
  )
}

LogNormal <- function(A_data,
                      B_data,
                      n_samples,
                      mu,
                      lambda,
                      alpha,
                      beta) {
  NormalResult <- Normal(log(A_data),
                         log(B_data),
                         n_samples,
                         mu,
                         lambda,
                         alpha,
                         beta)

  ## Means
  A_mus <- NormalResult$Mu$A
  B_mus <- NormalResult$Mu$B

  ## Sigmas
  A_sig_sqs <- NormalResult$Sig_Sq$A
  B_sig_sqs <- NormalResult$Sig_Sq$B

  ## Transform back to log normal for interpretation
  A_means <- exp(A_mus + A_sig_sqs / 2)
  B_means <- exp(B_mus + B_sig_sqs / 2)

  A_vars <- (exp(A_sig_sqs) - 1) * exp(2 * A_mus + A_sig_sqs)
  B_vars <- (exp(B_sig_sqs) - 1) * exp(2 * B_mus + B_sig_sqs)

  list(
    Mu = list(A = A_mus, B = B_mus),
    Sig_Sq = list(A = A_sig_sqs, B = B_sig_sqs),
    Mean = list(A = A_means, B = B_means),
    Var = list(A = A_vars, B = B_vars)
  )
}

Poisson <- function(A_data,
                    B_data,
                    n_samples,
                    shape,
                    rate) {
  map <- function(data) rgamma(n_samples, sum(data) + shape, length(data) + rate)
  list(
    Lambda = list(A = map(A_data), B = map(B_data))
  )
}

PoissonClosed <- function(A_data,
                          B_data,
                          shape,
                          rate) {
  prob <- PoissonClosed_(sum(A_data) + shape,
                         length(A_data) + rate,
                         sum(B_data) + shape,
                         length(B_data) + rate)
  list(Lambda = prob)
}

Uniform <- function(A_data,
                    B_data,
                    n_samples,
                    xm,
                    alpha) {
  map <- function(data) rpareto(n_samples, max(data, xm), length(data) + alpha)
  list(
    Theta = list(A = map(A_data), B = map(B_data))
  )
}
