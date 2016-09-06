#' @export
combine <- function(bT1, bT2, f = `+`, params, newName = 'Parameter') {
  
  if(length(params) != 2) stop('You must specify only (2) params. One for the first test and one for the second test.')
  
  if(!((params[1] %in% names(bT1$posteriors)) & (params[2] %in% names(bT2$posteriors)))) {
    stop("You have specified a `param` name that doesn't exist in the posterior of the first test and/or the second test.")
  }
  
  if(bT1$inputs$n_samples != bT1$inputs$n_samples) warning("n_samples not equal. Make sure `f` handles recycling appropriately.")
  
  A1 <- bT1$posteriors[[params[1]]][[1]]; A2 <- bT2$posteriors[[params[2]]][[1]]
  B1 <- bT1$posteriors[[params[1]]][[2]]; B2 <- bT2$posteriors[[params[2]]][[1]]
  
  result <- list()
  
  result$inputs <- list(A_data = list(bT1$inputs$A_data, bT2$inputs$A_data),
                         B_data = list(bT1$inputs$B_data, bT2$inputs$B_data),
                         priors = 'Combined distributions have no priors. Inspect each element separately for details.',
                         n_samples = c(bT1$inputs$n_samples, bT2$inputs$n_samples))
  
  result$posteriors[[newName]] <- list(A = f(A1, A2), B = f(B1, B2))
  
  result$distribution <- 'combined'
  
  class(result) <- c('bayesTest')
  
  return(result)
  
}
