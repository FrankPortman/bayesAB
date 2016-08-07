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
    plotNormalSamples(pos$A_mus, pos$B_mus, pos$A_sig_sqs, pos$B_sig_sqs, result$inputs$percent_lift)
    
    
  } else if(is(result, 'bayesLogNormalTest')) {
    
    ## Plot the posteriors
    pos <- result$posteriors
    
    plotLogNormalPosteriors(pos, result$trans_inputs$alphas, result$trans_inputs$betas)
    
    plotLogNormalSamples(pos, result$inputs$percent_lift)
    
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


print.bayesBernoulliTest <- function(result) {
  
  cat('Results of the Experiment: \n \n')
  cat('Clicks in the Test: ', sum(result$inputs$A_data), '\n', sep = "")
  cat('Views in the Test: ', length(result$inputs$A_data), '\n', sep = "")
  cat('Clicks in the Control: ', sum(result$inputs$B_data), '\n', sep = "")
  cat('Views in the Control: ', length(result$inputs$B_data), '\n', sep = "")
  cat('\n')
  cat('using a Beta(', result$inputs$alpha, ',', result$inputs$beta, ') prior.\n')
  
  cat('--------------------------------------------\n')
  
  cat('P(Test > Control) by at least ', result$inputs$percent_lift, '% = ', result$prob, '\n', sep = "")
  
}

print.minLift <- function(result) {
  
  print(get(result$test))
  
  cat('\n')
  cat('--------------------------------------------\n')
  cat(paste0('Maximum Lift that returns a ', paste(result$probability * 100, collapse = "%, ")))
  cat(paste0('% result that ', gsub(".$","",result$testVars[1]), ' > ', gsub(".$","",result$testVars[2]), ' is '))
  cat(paste(round(result$minLift * 100, 4), collapse = "%, "), '%.\n', sep = "")
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
