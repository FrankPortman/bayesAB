bayesPropTest <- function(clicks_test, views_test, clicks_control, views_control, alpha = 1, beta = 1, percent_lift = 0, N_samp = 1e6) {
  
  test_samples <- rbeta(N_samp, clicks_test + alpha, views_test - clicks_test + beta)
  control_samples <- rbeta(N_samp, clicks_control + alpha, views_control - clicks_control + beta)
  
  prob <- get.prob(test_samples, control_samples, percent_lift = percent_lift)
  
  result <- list(prob = prob,
                 test_samples = test_samples,
                 control_samples = control_samples,
                 alpha = alpha,
                 beta = beta,
                 percent_lift = percent_lift,
                 
                 inputs = list(
                   clicks_test = clicks_test,
                   views_test = views_test,
                   clicks_control = clicks_control,
                   views_control = views_control,
                   N_samp = N_samp
                 ),
                 
                 posteriors = list(
                   control_alpha = clicks_control + alpha,
                   control_beta = views_control - clicks_control + beta,
                   test_alpha = clicks_test + alpha,
                   test_beta = views_test - clicks_test + beta)
                 
  )
  
  class(result) <- c('bayesPropTest','bayesTest')
  
  return(result)
  
}

get.prob <- function(A_samples, B_samples, percent_lift = 0) {
  
  prob <- mean((100 * (A_samples - B_samples) / B_samples > percent_lift))
  
  prob
  
}

