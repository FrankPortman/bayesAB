bayesAB.env <- new.env()
bayesAB.env$functions <- list("bernoulli" = bayesBernoulliTest,
                              "normal" = bayesNormalTest,
                              "lognormal" = bayesLogNormalTest)

bayesTest <- function(A_data,
                      B_data,
                      priors,
                      percent_lift = 0,
                      n_samples = 1e5,
                      distribution) {
  
  if(!distribution %in% names(bayesAB.env$functions)) stop("Did not specify a valid distribution.")
  do.call(bayesAB.env$functions[distribution], list(A_data, B_data, priors, percent_lift, n_samples))
  
}
