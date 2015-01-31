plot.bayesPropTest <- function(result) {    

  ## Create data frame for 
  df <- data.frame(Test = result$test_samples, Control = result$control_samples)
  df <- reshape2::melt(df, id.vars = NULL)
  
  par(ask = TRUE)
  
  ## Plot the prior
  plotBeta(result$alpha, result$beta)
  
  ## Plot the results post simulation
  posterior <- ggplot2::ggplot(df, ggplot2::aes(x = value, group = variable, fill = variable)) + 
                  ggplot2::geom_density() +
                  ggplot2::xlab(NULL) +
                  ggplot2::ylab('Density') +
                  ggplot2::ggtitle('Test and Control PDFs')
  
  print(posterior)
  
  ## Plot the test samples minus the control samples
  testResult <- ggplot2::qplot(result$test_samples - result$control_samples, binwidth = diff(range(result$test_samples - result$control_samples)) / 50) +
                    ggplot2::xlab('Test Samples - Control Samples') +
                    ggplot2::ylab('Samples') +
                    ggplot2::ggtitle('Histogram of Test - Control Probability') +
                    ggplot2::geom_vline(x = 0)
  
  print(testResult)
  
  par(ask = FALSE)
  
}

print.bayesPropTest <- function(result) {
  
  cat('Results of the Experiment: \n \n')
  cat('Clicks in the Test: ', test$inputs$clicks_test, '\n', sep = "")
  cat('Views in the Test: ', test$inputs$views_test, '\n', sep = "")
  cat('Clicks in the Control: ', test$inputs$clicks_control, '\n', sep = "")
  cat('Views in the Control: ', test$inputs$views_control, '\n', sep = "")
  cat('\n')
  cat('using a Beta(', result$alpha, ',', result$beta, ') prior.\n')
  
  cat('--------------------------------------------\n')
  
  cat('P(Control > Test) by at least ', result$percent_lift, '% = ', result$prob, '\n', sep = "")
  
}

