bayesTest <- function(A_data,
                      B_data,
                      priors,
                      percent_lift = 0,
                      n_samples = 1e5,
                      distribution) {
  
  if(distribution == 'bernoulli') bayesBernoulliTest(A_data, B_data, priors, percent_lift, n_samples)
  else if(distribution == 'normal') bayesNormalTest(A_data, B_data, priors, percent_lift, n_samples)
  else if(distribution == 'lognormal') bayesLogNormalTest(A_Data, B_data, priors, percent_lift, n_samples)
  else stop("Did not specify a valid distribution.")
  
}

t <- bayesTest(1, 2, 3, 4, 5, "hi")
