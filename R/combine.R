#' Combine two \code{bayesAB} objects given a binary function.
#' 
#' @description Combine two (or any number, in succesion) \code{bayesTest} objects into a new arbitrary posterior distribution.
#'              The resulting object is of the same class.
#' 
#' @param bT1 a bayesTest object
#' @param bT2 a bayesTest object
#' @param f a binary function (f(x, y)) used to combine posteriors from bT1 to bT2
#' @param params a character vector of length 2, corresponding to names of the posterior parameters you want to combine
#' @param newName a string indicating the name of the new 'posterior' in the resulting object
#' 
#' @return a \code{bayesTest} object with the newly combined posterior samples
#' 
#' @examples 
#' A_binom <- rbinom(100, 1, .5)
#' B_binom <- rbinom(100, 1, .6)
#' 
#' A_norm <- rnorm(100, 6, 1.5)
#' B_norm <- rnorm(100, 5, 2.5)
#' 
#' AB1 <- bayesTest(A_binom, B_binom, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
#' AB2 <- bayesTest(A_norm, B_norm, 
#'                 priors = c('m0' = 5, 'k0' = 1, 's_sq0' = 3, 'v0' = 1), distribution = 'normal')
#' 
#' AB3 <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation')
#' 
#' print(AB3)
#' summary(AB3)
#' plot(AB3)
#' @export
combine <- function(bT1, bT2, f = `+`, params, newName = 'Parameter') {
  
  if(any(isClosed(bT1$distribution), isClosed(bT2$distribution))) stop("Can't combine a 'closed' bayesTest.")
  
  if(length(params) != 2) stop('You must specify only (2) params. One for the first test and one for the second test.')
  
  if(!((params[1] %in% names(bT1$posteriors)) & (params[2] %in% names(bT2$posteriors)))) {
    stop("You have specified a `param` name that doesn't exist in the posterior of the first test and/or the second test.")
  }
  
  if(bT1$inputs$n_samples != bT2$inputs$n_samples) warning("n_samples not equal. Make sure `f` handles recycling appropriately.")
  
  A1 <- bT1$posteriors[[params[1]]][[1]]; A2 <- bT2$posteriors[[params[2]]][[1]]
  B1 <- bT1$posteriors[[params[1]]][[2]]; B2 <- bT2$posteriors[[params[2]]][[2]]
  
  result <- list()
  
  result$inputs <- list(A_data = listConcat(listOr(bT1$inputs$A_data), listOr(bT2$inputs$A_data)),
                         B_data = listConcat(listOr(bT1$inputs$B_data), listOr(bT2$inputs$B_data)),
                         priors = 'Combined distributions have no priors. Inspect each element separately for details.',
                         n_samples = max(bT1$inputs$n_samples, bT2$inputs$n_samples))
  
  result$posteriors[[newName]] <- list(A = f(A1, A2), B = f(B1, B2))
  
  result$distribution <- 'combined'
  
  class(result) <- c('bayesTest')
  
  return(result)
  
}
