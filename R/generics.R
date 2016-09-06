#' @export
plot.bayesTest <- function(x, 
                           percentLift = rep(0, length(x$posteriors)),
                           priors = TRUE,
                           posteriors = TRUE,
                           samples = TRUE,
                           ...) {
  
  if(length(x$posteriors) != length(percentLift)) stop("Must supply a 'percentLift' for every parameter with a posterior distribution.")
  if(!any(priors, posteriors, samples)) stop("Must specifiy at least one plot to make.")
  
  oldPar <- par()$ask
  par(ask = TRUE)
  
  if(priors) plotPriors(x, ...) # for changing p, for some priorPlot params
  if(posteriors) plotPosteriors(x)
  if(samples) plotSamples(x, percentLift)
  
  par(ask = oldPar)
  
}

#' @export
print.bayesTest <- function(x, ...) {
  
  cat('--------------------------------------------\n')
  cat("Distribution used: ")
  cat(x$distribution, '\n')
  
  cat('--------------------------------------------\n')
  cat('Using data with the following properties: \n')
  print(cbind(A_data = sapply(x$inputs$A_data, summary), B_data = sapply(x$inputs$B_data, summary)))
  
  cat('--------------------------------------------\n')
  cat('Priors used for the calculation: \n')
  print(x$inputs$priors)
  
  cat('--------------------------------------------\n')
  cat('Calculated posteriors for the following parameters: \n')
  cat(paste0(names(x$posteriors), collapse = ", "), '\n')
  
  cat('--------------------------------------------\n')
  cat('Monte Carlo samples generated per posterior: \n')
  print(x$inputs$n_samples)
  
}

#' @export
summary.bayesTest <- function(object, 
                              percentLift = rep(0, length(object$posteriors)),
                              credInt = rep(.9, length(object$posteriors)),
                              ...) {
  
  if(length(object$posteriors) != length(percentLift)) stop("Must supply a 'percentLift' for every parameter with a posterior distribution.")
  if(length(object$posteriors) != length(credInt)) stop("Must supply a 'credInt' for every parameter with a posterior distribution.")
  
  
  probability <- Map(function(x, y) getProb(x[[1]], x[[2]], y), object$posteriors, percentLift)
  interval <- Map(function(x, y) getCredInt(x[[1]], x[[2]], y), object$posteriors, credInt)

  out <- list(probability = probability, 
              interval = interval, 
              percentLift = percentLift, 
              credInt = credInt)

  class(out) <- 'summaryBayesTest'
  
  return(out)
  
}

#' @export
print.summaryBayesTest <- function(x, ...) {
  
  cat('P(A > B) by (', paste0(x$percentLift, collapse = ", "), ')%: \n', sep = "")
  print(x$probability)
  
  cat('--------------------------------------------\n\n')
  
  cat('Credible Interval on (A - B) / B for interval length(s) (', paste0(x$credInt, collapse = ", "), ') : \n', sep = "")
  print(x$interval)

}

#' @export
print.bayesTestClosed <- function(x, ...) {
  
  
}
