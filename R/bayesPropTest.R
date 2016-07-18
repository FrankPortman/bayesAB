bayesBernoulliTest <- function(A_data,
                          B_data,
                          alpha = 1, 
                          beta = 1, 
                          percent_lift = 0, 
                          N_samp = 1e6) {
  
  if(!(any(A_data %in% c(0, 1),
           B_data %in% c(0, 1)))) {
    stop("Data input is incorrect. Data can only contain 0's and 1's. See help docs for more info.")
  }
  
  clicks_test <- sum(A_data)
  views_test <- length(A_data)
  
  clicks_control <- sum(B_data)
  views_control <- length(B_data)
  
  test_samples <- rbeta(N_samp, clicks_test + alpha, views_test - clicks_test + beta)
  control_samples <- rbeta(N_samp, clicks_control + alpha, views_control - clicks_control + beta)
  
  prob <- getProb(test_samples, control_samples, percent_lift = percent_lift)
  
  result <- list(prob = prob,
                 test_samples = test_samples,
                 control_samples = control_samples,
                 alpha = alpha,
                 beta = beta,
                 percent_lift = percent_lift,
                 
                 inputs = list(
                   A_data = A_data,
                   B_data = B_data,
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
  
  class(result) <- c('bayesBernoulliTest','bayesTest')
  
  return(result)
  
}
