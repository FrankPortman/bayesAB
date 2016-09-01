plotPriors <- function(bayesAB) {
  
  funs <- list("beta" = list(params = c('alpha', 'beta'), plotFun = plotBeta),
               "normal" = list(params = c('m0', 'k0'), plotFun = plotNormal),
               "invgamma" = list(params = c('s_sq0', 'v0'), plotFun = plotInvGamma),
               "gamma" = list(params = c('shape', 'rate'), plotFun = plotGamma))
  
  vals <- bayesAB$inputs$priors
  labs <- names(vals)
  
  labChecker <- function(...) all(c(...) %in% labs) 
  
  for(rel in funs) {
    if(labChecker(rel$params)) do.call(rel$plotFun, as.list(unname(vals[rel$params])))
  }
  
}
