bayesPropTestClosed <- function(clicks_test, views_test, clicks_control, views_control, percent_lift = 0) {
  
  alpha_1 <- clicks_test + 1
  beta_1 <- views_test + 1
  
  alpha_2 <- round(clicks_control * (1 + percent_lift / 100))
  alpha_2 <- alpha_2 + 1
  beta_2 <- views_control + 1
  
  prob <- alt_prop(alpha_1, beta_1, alpha_2, beta_2)
    
  result <- list(prob = prob,
                 percent_lift = percent_lift,
                 inputs = list(
                   clicks_test = clicks_test,
                   views_test = views_test,
                   clicks_control = clicks_control,
                   views_control = views_control
                 )
                 
  )
  
  class(result) <- 'bayesPropTestClosed'
  
  return(result)
  
}