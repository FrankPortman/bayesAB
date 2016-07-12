plot.bayesTest <- function(result) {    
  
  par(ask = TRUE)
  
  if (is(result,'bayesPropTest')) {
    
    ## Plot the prior
    plotBeta(result$alpha, result$beta)
    
    ## Plot the posteriors
    pos <- result$posteriors
    plotPosteriors(pos$control_alpha, pos$control_beta, pos$test_alpha, pos$test_beta)
    
    ## Plot the samples
    plotSamples(result$test_samples, result$control_samples, result$inputs, result$percent_lift)
    
  } else if (is(result,'bayesNormalTest')) {
    
    ## Plot the posteriors
    pos <- result$posteriors
    plotNormalPosteriors(pos$A_mus, pos$B_mus, pos$A_sig_sqs, pos$B_sig_sqs, pos$alphas, pos$betas)
    
    ## Plot the samples
    plotNormalSamples(result$posteriors$A_mus, result$posteriors$B_mus, result$posteriors$A_sig_sqs, result$posteriors$B_sig_sqs)
    
    
  } else if(is(result, 'bayesLogNormalTest')) {
    
    ## Plot the posteriors
    pos <- result$posteriors
    
    plotLogNormalPosteriors(pos$A_mus, pos$B_mus, pos$A_sig_sqs, pos$B_sig_sqs, pos$statistics, pos$alphas, pos$betas)
    
  } else if(is(result, 'bayesNegBinTest')) {
    
    ## Plot the posteriors
    pos <- result$posteriors
    
    plotNegBinPosteriors(pos$A_mean, pos$B_mean, pos$A_prob, pos$B_prob, pos$A_r, pos$B_r, pos$A_var, pos$B_var)
  }
  
  
  par(ask = FALSE)
  
}


plotSamples <- function(test_samples, control_samples, inputs, percent_lift) {
  
  ratio <- inputs$clicks_control / inputs$views_control
  cutoff <- percent_lift / 100
  
  diff <- (test_samples - control_samples) / control_samples
  diff <- data.frame(diff = diff, cutoff = diff < cutoff)
  
  prop <- 1 - sum(diff$cutoff) / nrow(diff)
  prop <- round(prop * 100, digits = 1)
  
  p <- ggplot2::qplot(diff, data = diff, fill = cutoff, binwidth = diff(range(diff)) / 250) + 
    ggplot2::geom_vline(xintercept = cutoff)
  
  m <- max(ggplot2::ggplot_build(p)$panel$ranges[[1]]$y.range)
  
  p <- p + ggplot2::annotate('text', x = mean(diff$diff[diff$cutoff == F]), y = m / 3, label = paste(prop, '%', sep = "")) +
    ggplot2::xlab('(Test Samples - Control Samples) / Control Samples') +
    ggplot2::ylab('Samples of Beta Distribution') +
    ggplot2::ggtitle('Histogram of (Test - Control / Control) Values') +
    ggplot2::theme(legend.position = "none")
  
  print(p) 
  
}

plotNormalSamples <- function(test_mus, control_mus, test_vars, control_vars, cutoff = 0) {
  #Mu:
  diff <- test_mus - control_mus
  diff <- data.frame(diff = diff, cutoff = diff < cutoff)
  
  prop <- 1 - sum(diff$cutoff) / nrow(diff)
  prop <- round(prop * 100, digits = 1)
  
  p <- ggplot2::qplot(diff, data = diff, fill = cutoff, binwidth = diff(range(diff)) / 250) + 
    ggplot2::geom_vline(xintercept = cutoff)
  
  m <- max(ggplot2::ggplot_build(p)$panel$ranges[[1]]$y.range)
  
  p <- p + ggplot2::annotate('text', x = mean(diff$diff[diff$cutoff == F]), y = m / 3, label = paste(prop, '%', sep = "")) +
    ggplot2::xlab('Test Samples - Control Samples') +
    ggplot2::ylab('Samples of Mu Distribution') +
    ggplot2::ggtitle('Histogram of Test - Control Probability') +
    ggplot2::theme(legend.position = "none")
  
  print(p) 
  
  #Variance:
  diff <- test_vars - control_vars
  diff <- data.frame(diff = diff, cutoff = diff < cutoff)
  
  prop <- 1 - sum(diff$cutoff) / nrow(diff)
  prop <- round(prop * 100, digits = 1)
  
  p <- ggplot2::qplot(diff, data = diff, fill = cutoff, binwidth = diff(range(diff)) / 250) + 
    ggplot2::geom_vline(xintercept = cutoff)
  
  m <- max(ggplot2::ggplot_build(p)$panel$ranges[[1]]$y.range)
  
  p <- p + ggplot2::annotate('text', x = mean(diff$diff[diff$cutoff == F]), y = m / 3, label = paste(prop, '%', sep = "")) +
    ggplot2::xlab('Test Variance Samples - Control Variance Samples') +
    ggplot2::ylab('Samples of Variance Distribution') +
    ggplot2::ggtitle('Histogram of Test - Control Variance Probability') +
    ggplot2::theme(legend.position = "none")
  
  print(p) 
}

plotLogNormalSamples <- function(test_mus,
                                 control_mus,
                                 test_vars,
                                 control_vars,
                                 other_statistics,
                                 cutoff = 0) {
  
  
  plotNormalSamples(test_mus, control_mus, test_vars, control_vars, cutoff)
  
  
  
  
}

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

plotPosteriors <- function(control_alpha, control_beta, test_alpha, test_beta) {
  
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

plotNegBinPosteriors <- function(A_mean, B_mean, A_prob, B_prob, A_r, B_r, A_var, B_var) {
  
  dat <- reshape2::melt(cbind(A_mean,B_mean))
  mean_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Mean Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(mean_posteriors)
  
  dat <- reshape2::melt(cbind(A_prob,B_prob))
  prob_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Prob Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(prob_posteriors)
  
  dat <- reshape2::melt(cbind(A_r,B_r))
  r_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control r Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(r_posteriors)
  
  dat <- reshape2::melt(cbind(A_var,B_var))
  var_posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x=value, group = Var2, fill=Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Variance Posteriors') +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(var_posteriors)
}


print.bayesPropTest <- function(result) {
  
  cat('Results of the Experiment: \n \n')
  cat('Clicks in the Test: ', result$inputs$clicks_test, '\n', sep = "")
  cat('Views in the Test: ', result$inputs$views_test, '\n', sep = "")
  cat('Clicks in the Control: ', result$inputs$clicks_control, '\n', sep = "")
  cat('Views in the Control: ', result$inputs$views_control, '\n', sep = "")
  cat('\n')
  cat('using a Beta(', result$alpha, ',', result$beta, ') prior.\n')
  
  cat('--------------------------------------------\n')
  
  cat('P(Test > Control) by at least ', result$percent_lift, '% = ', result$prob, '\n', sep = "")
  
}

print.minLift <- function(result) {
  
  print(get(result$test))
  
  cat('\n')
  cat('--------------------------------------------\n')
  cat('Maximum Lift that returns a ', result$probability * 100, '% (+- ', result$threshold, ') result is ', result$minLift, '%.\n', sep = "")
  cat('P(Test > Control) by at least ', result$minLift, '% = ', result$actualProb, '\n', sep = "")
  cat('Access directly with $minLift\n', sep = "")
  
}

