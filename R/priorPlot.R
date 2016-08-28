priorPlot.env <- new.env()
priorPlot.env$mappings <- list("beta" = list(params = c('alpha', 'beta'), plotFun = plotBeta),
                               "normal" = list(params = c('m0', 'k0'), plotFun = plotNormal),
                               "invgamma" = list(params = c('s_sq0', 'v0'), plotFun = plotInvGamma))

plotPriors <- function(bayesAB) {
  
  vals <- bayesAB$inputs$priors
  labs <- names(vals)
  
  labChecker <- function(...) all(c(...) %in% labs) 
  
  for(rel in priorPlot.env$mappings) {
    if(labChecker(rel$params)) do.call(rel$plotFun, as.list(unname(vals[rel$params])))
  }
  
}
