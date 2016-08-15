## Plot stays the same except percentLift now goes into the plots
#' @export
plot.bayesTest <- function(x, percentLift = rep(0, length(x$posteriors))) {
  
  if(length(x$posteriors) != length(percentLift)) stop("Must supply a 'percentLift' for every parameter with a posterior distribution.")
  
  oldPar <- par()
  par(ask = TRUE)
  
  #plotPriors(x)
  plostPosteriors(x)
  plostSamples(x, percentLift)
  
  par(oldPar)
  
}

## Print just prints about the test, summarize input data streams, and talk about priors, n_samp, etc
#' @export
print.bayesTest <- function(x, ...) {
  
  cat('--------------------------------------------\n')
  cat('Using data with the following properties: \n')
  print(cbind(A_data = summary(x$inputs$A_data), B_data = summary(x$inputs$B_data)))
  
  cat('--------------------------------------------\n')
  cat('Priors used for the calculation: \n')
  print(x$inputs$priors)
  
  cat('--------------------------------------------\n')
  cat('Summaries of the posteriors: \n')
  print(sapply(x$posteriors, summary))
  
  cat('--------------------------------------------\n')
  
  #Warning: assumes format for posteriors is A_param1, B_param1, A_param2, B_param2, etc.
  probAgreatB <- sapply(1:(length(x$posteriors)/2), 
               function(x) mean((x$posteriors[[2*x-1]] - x$posteriors[[2*x]]) / (x$posteriors[[2*x]]) > 0))
  varNames <- names(x$posteriors[seq(2,length(x$posteriors),2)])
  varNames <- sapply(varNames, function(x) substr(x, 3, nchar(x)))
  names(probAgreatB) <- varNames
  
  cat('P(A > B) for the following posteriors: \n')
  print(probAgreatB)
}

## does P(A>B) for percentLift across all params and whatever else
summary.bayesTest <-function(x, percentLift = rep(0, length(x$posteriors))) {
  
  if(length(x$posteriors) != length(percentLift)) stop("Must supply a 'percentLift' for every parameter with a posterior distribution.")
  
}


# print.bayesBernoulliTest <- function(x) {
# 
#   cat('xs of the Experiment: \n \n')
#   cat('Clicks in A: ', sum(x$inputs$A_data), '\n', sep = "")
#   cat('Views in A: ', length(x$inputs$A_data), '\n', sep = "")
#   cat('Clicks in B: ', sum(x$inputs$B_data), '\n', sep = "")
#   cat('Views in B: ', length(x$inputs$B_data), '\n', sep = "")
#   cat('\n')
#   cat('using a Beta(', as.numeric(x$inputs$priors['alpha']), ',', as.numeric(x$inputs$priors['beta']), ') prior.\n')
# 
#   cat('--------------------------------------------\n')
# 
#   cat('P(A > B) by at least ', x$inputs$percent_lift, '% = ', x$prob, '\n', sep = "")
# 
# }

#' @export
print.minLift <- function(x, ...) {
  
  print(get(x$test))
  
  cat('\n')
  cat('--------------------------------------------\n')
  cat(paste0('Maximum Lift that returns a ', paste(x$probability * 100, collapse = "%, ")))
  cat(paste0('% x that ', gsub(".$","",x$testVars[1]), ' > ', gsub(".$","",x$testVars[2]), ' is '))
  cat(paste(round(x$minLift * 100, 4), collapse = "%, "), '%.\n', sep = "")
  cat('Access directly with $minLift\n', sep = "")
}

#' @export
`+.bayesTest` <- function(e1, e2) {
  
  if(e1$inputs$n_samples != e2$inputs$n_samples) warning("n_samples not equal. Recycling elements for target distribution.")
  
  #add all of e1A to all of e2A
  #add all of e1B to all of e2B
  
}

#' @export
`*.bayesTest` <- function(e1, e2) {
  
  if(e1$inputs$n_samples != e2$inputs$n_samples) warning("n_samples not equal. Recycling elements for target distribution.")
  
  #multiply all of e1A to all of e2A
  #multiply all of e1B to all of e2B
  
} 
