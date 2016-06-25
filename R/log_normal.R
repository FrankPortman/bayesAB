# draw_mus_and_sigmas <- function(data, m0, k0, s_sq0, v0, n_samples = 10000) {
#   
#   N <- length(data)
#   the_mean <- mean(data)
#   
#   ssd <- sum((data - the_mean) ^ 2)
#   
#   kN <- k0 + N
#   mN <- (k0 / kN) * m0 + (N / kN) * the_mean
#   vN <- v0 + N
#   
#   vN_times_s_sqN <- v0 * s_sq0 + ssd + (N * k0 * (m0 - the_mean) ^ 2) / kN
#   
#   
#   alpha <- vN / 2
#   beta <- vN_times_s_sqN / 2
#   
#   sig_sq_samples <- (1 / rgamma(n_samples, alpha, beta))
#   
#   mean_norm <- mN
#   var_norm <- sqrt(sig_sq_samples / kN)
#   mu_samples <- rnorm(n_samples, mean_norm, var_norm)
#   
#   return(list(mu_samples = mu_samples,
#               sig_sq_samples = sig_sq_samples))
#   
# }
# 
# bayesNormalTest <- function(A_data,
#                             B_data,
#                             m0,
#                             k0,
#                             s_sq0,
#                             v0,
#                             n_samples = 1e5) {
#   
#   
#   A <- draw_mus_and_sigmas(A_data, m0, k0, s_sq0, v0)
#   B <- draw_mus_and_sigmas(B_data, m0, k0, s_sq0, v0)
#   
#   A_mus <- A$mu_samples
#   B_mus <- B$mu_samples
#   
#   A_sig_sqs <- A$sig_sq_samples
#   B_sig_sqs <- B$sig_sq_samples
#   
#   result <- list(mu_prob = mean(A_mus > B_mus),
#                  sig_sq_prob = mean(A_sig_sqs > B_sig_sqs),
#                  
#                  inputs = list(
#                    A_data = A_data,
#                    B_data = B_data,
#                    m0 = m0,
#                    k0 = k0,
#                    s_sq0 = s_sq0,
#                    v0 = v0,
#                    n_samples = n_samples
#                  ),
#                  
#                  posteriors = list(
#                    A_mus = A_mus,
#                    B_mus = B_mus,
#                    A_sig_sqs = A_sig_sqs,
#                    B_sig_sqs = B_sig_sqs
#                  ))
#   
#   
#   
# }
# 
# ggplot(faithful, aes(waiting, eruptions)) +
#   geom_density_2d()
# 
# 
# v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
# v + geom_contour()
# 
# xs <- seq(mean_norm - max(var_norm) * 2, mean_norm + max(var_norm) * 2, .01)
# 
# mu1 <- lapply(var_norm, function(x) dnorm(xs, mean_norm, x))
# 
# library(reshape2)
# 
# dat <- melt(mu1)
# dat$xs <- xs
# dat$vars <- rep(var_norm, each = length(xs))
# 
# ggplot(dat, aes(xs, value, z = vars)) + stat_density_2d()
# 
# ggplot(dat, aes(xs, mu1, ))
# 
# library(akima)
# 
# fld <- interp(dat$xs,dat$value,dat$vars)
# filled.contour(fld)
# 
# seq <- seq(0, 9.999, .0001)
# 
# Test <- dnorm(seq, mean_norm, var_norm)
# Control <- dbeta(seq, control_alpha, control_beta)
# 
# dat <- reshape2::melt(cbind(Test, Control))
# dat <- cbind(dat, seq)
# 
# m0 <- 4
# k0 <- 1
# s_sq0 <- 1
# v0 <- 1
# 
# A_data <- rnorm(1000, 4.1, 3)
# B_data <- rnorm(1000, 4.0, 3)
# 
# A <- draw_mus_and_sigmas(A_data, m0, k0, s_sq0, v0)
# B <- draw_mus_and_sigmas(B_data, m0, k0, s_sq0, v0)
# 
# A_mus <- A$mu_samples
# A_sig_sqs <- A$sig_sq_samples
# 
# B_mus <- B$mu_samples
# B_sig_sqs <- B$sig_sq_samples
# 
# print(mean(A_mus > B_mus))
# 
# print(mean(A_sig_sqs > B_sig_sqs))
# 
# # step 4: perform numerical integration
# # probability that mean of A is greater than mean of B:
# print mean(A_mus > B_mus) 
# # probability that variance of A is greater than variance of B:
# print mean(A_sig_sqs > B_sig_sqs)
# 
# 
# ###
# 
# sig_sq_samples <- (1 / rgamma(n_samples, alpha, beta))
# 
# 
# xs <- seq(.5, 1.1, .001)
# 
# xs <- seq(0, 2, .001)
# 
# thing <- dgamma(xs, alpha, beta)
# thing <- 1 / thing
# 
# qplot(xs, thing)
# 
# qplot(sig_sq_samples)
# 
# qplot(xs, dinvgamma(xs, alpha, beta))
# 
# qplot(thing, dinvgamma(xs, alpha, beta))
# 
# 
# dinvgamma <- function(x, shape, scale) {
#   
#   if (shape <= 0 | scale <= 0) {
#     stop("Shape or scale parameter negative in dinvgamma().\n")
#   }
#   
#   alpha <- shape
#   beta <- scale
#   log.density <- alpha * log(beta) - lgamma(alpha) - (alpha + 1) * log(x) - (beta / x)
#   
#   return(exp(log.density))
#   
# }
# 
# 
