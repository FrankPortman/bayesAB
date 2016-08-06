plot.bayesTest <- function(result) {    
  
  par(ask = TRUE)
  
  if (is(result,'bayesBernoulliTest')) {
    
    ## Plot the prior
    plotBeta(result$inputs$alpha, result$inputs$beta)
    
    ## Plot the posteriors
    pos <- result$posteriors
    plotBernoulliPosteriors(pos$control_alpha, pos$control_beta, pos$test_alpha, pos$test_beta)
    
    ## Plot the samples
    plotBernoulliSamples(pos$test_samples, pos$control_samples, result$inputs$percent_lift)
    
  } else if (is(result,'bayesNormalTest')) {
    
    ## Plot the posteriors
    pos <- result$posteriors
    plotNormalPosteriors(pos$A_mus, pos$B_mus, pos$A_sig_sqs, pos$B_sig_sqs, 
                         result$trans_inputs$alphas, result$trans_inputs$betas)
    
    ## Plot the samples
    plotNormalSamples(result$posteriors$A_mus, result$posteriors$B_mus, 
                      result$posteriors$A_sig_sqs, result$posteriors$B_sig_sqs, result$inputs$percent_lift)
    
    
  } else if(is(result, 'bayesLogNormalTest')) {
    
    ## Plot the posteriors
    pos <- result$posteriors
    
    plotLogNormalPosteriors(pos$A_mus, pos$B_mus, pos$A_sig_sqs, pos$B_sig_sqs, pos$statistics, 
                            result$trans_inputs$alphas, result$trans_inputs$betas)
    
  } else if(is(result, 'bayesNegBinTest')) {
    
    ## Plot the posteriors
    pos <- result$posteriors
    
    plotNegBinPosteriors(pos$A_mean, pos$B_mean, pos$A_prob, pos$B_prob, pos$A_r, pos$B_r, pos$A_var, pos$B_var)
  }
  
  
  par(ask = FALSE)
  
}

print.bayesTest <- function(result) {
  
  #lapply through all params
  
  
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


`+.bayesTest` <- function(e1, e2) {
  
  if(e1$inputs$n_samples != e2$inputs$n_samples) warning("n_samples not equal. Recycling elements for target distribution.")
  
  #add all of e1A to all of e2A
  #add all of e1B to all of e2B
  
}

`*.bayesTest` <- function(e1, e2) {
  
  if(e1$inputs$n_samples != e2$inputs$n_samples) warning("n_samples not equal. Recycling elements for target distribution.")
  
  #multiply all of e1A to all of e2A
  #multiply all of e1B to all of e2B
  
} 
