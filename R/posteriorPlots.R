plotLogNormalPosteriors <- function(A_mus,
                                    B_mus, 
                                    A_sig_sqs, 
                                    B_sig_sqs, 
                                    other_statistics, 
                                    alphas, 
                                    betas) {
  
  plotNormalPosteriors(A_mus, B_mus, A_sig_sqs, B_sig_sqs, alphas, betas)
  
  
  ## Don't plot samples, plot transformations
  A_means <- other_statistics$A_means
  B_means <- other_statistics$B_means
  dat <- reshape2::melt(cbind(A_means, B_means))
  mean_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Mean Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(mean_posteriors)
  
  A_meds <- other_statistics$A_meds
  B_meds <- other_statistics$B_meds
  dat <- reshape2::melt(cbind(A_meds, B_meds))
  med_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Median Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(med_posteriors)
  
  A_modes <- other_statistics$A_modes
  B_modes <- other_statistics$B_modes
  dat <- reshape2::melt(cbind(A_modes, B_modes))
  mode_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Mode Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(mode_posteriors)
  
  A_vars <- other_statistics$A_vars
  B_vars <- other_statistics$B_vars
  dat <- reshape2::melt(cbind(A_vars, B_vars))
  var_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Variance Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(var_posteriors)
  
}

plotBernoulliPosteriors <- function(control_alpha, control_beta, test_alpha, test_beta) {
  
  seq <- seq(0, 1, .001)
  
  Test <- dbeta(seq, test_alpha, test_beta)
  Control <- dbeta(seq, control_alpha, control_beta)
  
  dat <- reshape2::melt(cbind(Test, Control))
  dat <- cbind(dat, seq)
  
  posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x = seq, y = value, group = Var2, fill = Var2)) + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = value), alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(posteriors)
  
}

plotNormalPosteriors <- function(A_mus, B_mus, A_sig_sqs, B_sig_sqs, alphas, betas) {
  
  dat <- reshape2::melt(cbind(A_mus,B_mus))
  mu_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Mu Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(mu_posteriors)
  
  l <- .01
  r <- max(qinvgamma(.995, alphas$A_alpha, betas$A_beta), qinvgamma(.995, alphas$B_alpha, betas$B_beta))
  step <- (r - l) / 1000
  
  support <- seq(l, r, step)
  sigma_a <- dinvgamma(support, alphas$A_alpha, betas$A_beta)
  sigma_b <- dinvgamma(support, alphas$B_alpha, betas$B_beta)
  
  dat <- reshape2::melt(cbind(sigma_a,sigma_b))
  dat <- cbind(dat, support)
  
  sigma_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=support, y=value, group = Var2, fill=Var2)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = value), alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Sigma Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(sigma_posteriors)
}