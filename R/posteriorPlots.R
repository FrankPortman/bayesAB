# plotLogNormalPosteriors <- function(A_mus,
#                                     B_mus, 
#                                     A_sig_sqs, 
#                                     B_sig_sqs, 
#                                     other_statistics, 
#                                     alphas, 
#                                     betas) {
#   
#   ## Plot Normal Posteriors first, since they are the same as the old method
#   plotNormalPosteriors(A_mus, B_mus, A_sig_sqs, B_sig_sqs, alphas, betas)
#   
#   ## Don't plot samples, plot transformations
#   
#   ## Means
#   A_means <- other_statistics$A_means
#   B_means <- other_statistics$B_means
#   dat <- reshape2::melt(cbind(A_means, B_means))
#   
#   posteriorPlotSamp(dat, "Mean")
#   
#   ## Medians
#   A_meds <- other_statistics$A_meds
#   B_meds <- other_statistics$B_meds
#   dat <- reshape2::melt(cbind(A_meds, B_meds))
#   
#   posteriorPlotSamp(dat, "Median")
#   
#   ## Modes
#   A_modes <- other_statistics$A_modes
#   B_modes <- other_statistics$B_modes
#   dat <- reshape2::melt(cbind(A_modes, B_modes))
#   
#   posteriorPlotSamp(dat, "Mode")
#   
#   ## Variance
#   A_vars <- other_statistics$A_vars
#   B_vars <- other_statistics$B_vars
#   dat <- reshape2::melt(cbind(A_vars, B_vars))
# 
#   posteriorPlotSamp(dat, "Variance")
#   
# }
# 
# plotBernoulliPosteriors <- function(control_alpha, control_beta, test_alpha, test_beta) {
#   
#   support <- seq(0, 1, .001)
#   
#   A_prob <- dbeta(seq, test_alpha, test_beta)
#   B_prob <- dbeta(seq, control_alpha, control_beta)
#   
#   dat <- reshape2::melt(cbind(A_prob, B_prob))
#   dat <- cbind(dat, support)
#   
#   posteriorPlotClosed(dat, "Probability")
#   
# }
# 
# plotPosteriors <- function(bayesAB) {
#   for(i in 1:length(bayesAB$posteriors)) {
#     obj <- bayesAB$posteriors[[i]]
#     posteriorPlotClosed(obj[[1]], obj[[2]], names(obj))
#   }
# }
# 
# plotNormalPosteriors <- function(A_mus, B_mus, A_sig_sqs, B_sig_sqs, alphas, betas) {
#   
#   ## Mus
#   dat <- reshape2::melt(cbind(A_mus,B_mus))
#   
#   posteriorPlotSamp(dat, "Mu")
#   
#   
#   ## Sigma ^ 2
#   ## Calculate bounds for inverse gamma distribution
#   l <- .01
#   r <- max(qinvgamma(.995, alphas$A_alpha, betas$A_beta), qinvgamma(.995, alphas$B_alpha, betas$B_beta))
#   step <- (r - l) / 1000
#   
#   support <- seq(l, r, step)
#   A_sigma <- dinvgamma(support, alphas$A_alpha, betas$A_beta)
#   B_sigma <- dinvgamma(support, alphas$B_alpha, betas$B_beta)
#   
#   dat <- reshape2::melt(cbind(A_sigma, B_sigma))
#   dat <- cbind(dat, support)
#   
#   posteriorPlotClosed(dat, "Sig_Sq")
#   
# }
# 
# # plotNegBinPosteriors <- function(A_mean, B_mean, A_prob, B_prob, A_r, B_r, A_var, B_var) {
# #   
# #   dat <- reshape2::melt(cbind(A_mean,B_mean))
# #   mean_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
# #     ggplot2::geom_density(alpha = 0.75) +
# #     ggplot2::xlab(NULL) +
# #     ggplot2::ylab('Density') +
# #     ggplot2::ggtitle('Test and Control Mean Posteriors') +
# #     ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
# #   
# #   print(mean_posteriors)
# #   
# #   dat <- reshape2::melt(cbind(A_prob,B_prob))
# #   prob_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
# #     ggplot2::geom_density(alpha = 0.75) +
# #     ggplot2::xlab(NULL) +
# #     ggplot2::ylab('Density') +
# #     ggplot2::ggtitle('Test and Control Prob Posteriors') +
# #     ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
# #   
# #   print(prob_posteriors)
# #   
# #   dat <- reshape2::melt(cbind(A_r,B_r))
# #   r_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
# #     ggplot2::geom_density(alpha = 0.75) +
# #     ggplot2::xlab(NULL) +
# #     ggplot2::ylab('Density') +
# #     ggplot2::ggtitle('Test and Control r Posteriors') +
# #     ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
# #   
# #   print(r_posteriors)
# #   
# #   dat <- reshape2::melt(cbind(A_var,B_var))
# #   var_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
# #     ggplot2::geom_density(alpha = 0.75) +
# #     ggplot2::xlab(NULL) +
# #     ggplot2::ylab('Density') +
# #     ggplot2::ggtitle('Test and Control Variance Posteriors') +
# #     ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
# #   
# #   print(var_posteriors)
# # }