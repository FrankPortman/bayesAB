draw_mus_and_sigmas <- function(data, m0, k0, s_sq0, v0, n_samples = 100) {
  
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
              sig_sq_samples = sig_sq_samples))
  
}

bayesNormalTest <- function(A_data,
                            B_data,
                            m0,
                            k0,
                            s_sq0,
                            v0,
                            n_samples = 1e5) {
  
  
  A <- draw_mus_and_sigmas(A_data, m0, k0, s_sq0, v0)
  B <- draw_mus_and_sigmas(B_data, m0, k0, s_sq0, v0)
  
  A_mus <- A$mu_samples
  B_mus <- B$mu_samples
  
  A_sig_sqs <- A$sig_sq_samples
  B_sig_sqs <- B$sig_sq_samples
  
  result <- list(mu_prob = mean(A_mus > B_mus),
                 sig_sq_prob = mean(A_sig_sqs > B_sig_sqs),
                 
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
                   B_sig_sqs = B_sig_sqs
                 ))
  
  
  
}

## mu prior
m0 <- 4
k0 <- 1
s_sq0 <- 1
v0 <- 1


plotBeta <- function(alpha, beta) {
  
  seq <- seq(0, 1, .001)
  hseq <- dbeta(seq, alpha, beta)
  
  p <- ggplot2::qplot(x = seq, y = hseq, geom = "line") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, .1)) +
    ggplot2::xlab(NULL) + 
    ggplot2::ylab('PDF') + 
    ggplot2::ggtitle(paste('Probability Density Function for Parameters: alpha = ', alpha, ', beta = ', beta, sep = ''))
  
  
  print(p)
  
}

plotNormal <- function(mu, s_sq) {
  
  support <- seq(mu - s_sq * 5, mu + s_sq * 5, .001)
  hseq <- dnorm(support, mu, s_sq)
  
  p <- ggplot2::qplot(x = support, y = hseq, geom = "line") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('PDF') +
    ggplot2::ggtitle(paste('Probability Density Function for Parameters: mu = ', mu, ', s_sq = ', s_sq, sep = ""))
  
  print(p)
  
}

dinvgamma <- function(x, shape, scale) {
  
  if (shape <= 0 | scale <= 0) {
    stop("Shape or scale parameter negative in dinvgamma().\n")
  }
  
  alpha <- shape
  beta <- scale
  log.density <- alpha * log(beta) - lgamma(alpha) - (alpha + 1) * log(x) - (beta / x)
  
  return(exp(log.density))
  
}

plotInvGamma <- function(shape, scale) {
  
  support <- seq(0, 50, .01)
  hseq <- dinvgamma(support, shape, scale)
  
  p <- ggplot2::qplot(x = support, y = hseq, geom = "line") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('PDF') +
    ggplot2::ggtitle(paste('Probability Density Function for Parameters: shape = ', shape, ', scale = ', scale, sep = ""))
  
  print(p)
  
}

plotNormal(5, 3)

plotInvGamma(1, 1)
plotInvGamma(1, 1)

plotInvGamma(4000, 8000)
plotInvGamma(2, 4)
plotInvGamma(1, 2)
plotInvGamma(1, 17)
plotInvGamma

