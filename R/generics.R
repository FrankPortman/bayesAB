plot.bayesPropTest <- function(result) {    

  ## Create data frame for 
#   df <- data.frame(Test = result$test_samples, Control = result$control_samples)
#   df <- reshape2::melt(df, id.vars = NULL)
  
  par(ask = TRUE)
  
  ## Plot the prior
  plotBeta(result$alpha, result$beta)
  
  ## Plot the results post simulation
#   posterior <- ggplot2::ggplot(df, ggplot2::aes(x = value, group = variable, fill = variable)) + 
#                   ggplot2::geom_density() +
#                   ggplot2::xlab(NULL) +
#                   ggplot2::ylab('Density') +
#                   ggplot2::ggtitle('Test and Control Posteriors')
#   
#   print(posterior)
  pos <- result$posteriors
  plotPosteriors(pos$control_alpha, pos$control_beta, pos$test_alpha, pos$test_beta)
  
  ## Plot the test samples minus the control samples
  testResult <- ggplot2::qplot(result$test_samples - result$control_samples, binwidth = diff(range(result$test_samples - result$control_samples)) / 50) +
                    ggplot2::xlab('Test Samples - Control Samples') +
                    ggplot2::ylab('Samples') +
                    ggplot2::ggtitle('Histogram of Test - Control Probability') +
                    ggplot2::geom_vline(x = 0)
  
  print(testResult)
  
  par(ask = FALSE)
  
}

plotPosteriors <- function(control_alpha, control_beta, test_alpha, test_beta) {
  
  seq <- seq(0, 1, .001)
  
  Test <- dbeta(seq, test_alpha, test_beta)
  Control <- dbeta(seq, control_alpha, control_beta)
  
  dat <- reshape2::melt(cbind(Test, Control))
  dat <- cbind(dat, seq)
  
  posteriors <- ggplot2::ggplot(dat, ggplot2::aes(x = seq, y = value, group = Var2, fill = Var2)) + 
    ggplot2::geom_ribbon(aes(ymin = 0, ymax = value)) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle('Test and Control Posteriors') +
    guides(fill=guide_legend(title=NULL))
    
  print(posteriors)
  
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

