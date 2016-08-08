plot.bayesTest <- function(result) {    
  
  par(ask = TRUE)
  
  if (is(result,'bayesBernoulliTest')) {
    
    ## Plot the prior
    bernPriors <- result$inputs$priors
    plotBeta(as.numeric(bernPriors['alpha']), as.numeric(bernPriors['beta']))
    
    ## Plot the posteriors
    pos <- result$posteriors
    plotBernoulliPosteriors(pos$B_alpha, pos$B_beta, pos$A_alpha, pos$A_beta)
    
    ## Plot the samples
    plotBernoulliSamples(pos$A_probs, pos$B_probs, result$inputs$percent_lift)
    
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
  
  cat('--------------------------------------------\n')
  cat('Using data with the following properties: \n')
  print(cbind(A_data = summary(AB1Norm$inputs$A_data), B_data = summary(AB1Norm$inputs$B_data)))
  
  cat('--------------------------------------------\n')
  cat('Priors used for the calculation: \n')
  print(result$inputs$priors)
  
  cat('--------------------------------------------\n')
  cat('Summaries of the posteriors: \n')
  print(sapply(result$posteriors, summary))
  
  cat('--------------------------------------------\n')
  
  #Warning: assumes format for posteriors is A_param1, B_param1, A_param2, B_param2, etc.
  probAgreatB <- sapply(1:(length(result$posteriors)/2), 
               function(x) mean((result$posteriors[[2*x-1]] - result$posteriors[[2*x]]) / (result$posteriors[[2*x]]) > 0))
  varNames <- names(result$posteriors[seq(2,length(result$posteriors),2)])
  varNames <- sapply(varNames, function(x) substr(x, 3, nchar(x)))
  names(probAgreatB) <- varNames
  
  cat('P(A > B) for the following posteriors: \n')
  print(probAgreatB)
}


# print.bayesBernoulliTest <- function(result) {
# 
#   cat('Results of the Experiment: \n \n')
#   cat('Clicks in A: ', sum(result$inputs$A_data), '\n', sep = "")
#   cat('Views in A: ', length(result$inputs$A_data), '\n', sep = "")
#   cat('Clicks in B: ', sum(result$inputs$B_data), '\n', sep = "")
#   cat('Views in B: ', length(result$inputs$B_data), '\n', sep = "")
#   cat('\n')
#   cat('using a Beta(', as.numeric(result$inputs$priors['alpha']), ',', as.numeric(result$inputs$priors['beta']), ') prior.\n')
# 
#   cat('--------------------------------------------\n')
# 
#   cat('P(A > B) by at least ', result$inputs$percent_lift, '% = ', result$prob, '\n', sep = "")
# 
# }

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
