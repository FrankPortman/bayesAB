bayes.prop.test <- function(clicks_a, views_a, clicks_b, views_b, alpha = 1, beta = 1, percent_lift = 0, N_samp = 1e6, min_lift = FALSE) {
  
  test_samples <- rbeta(N_samp, clicks_a + alpha, views_a - clicks_a + beta)
  test_samples <- rbeta(N_samp, clicks_b + alpha, views_b - clicks_b + beta)
  
  prob <- get.prob(A_samples, B_samples, percent_lift = percent_lift)
  
  ans <- 0
  
  if(min_lift) {
    
    ans <- min.lift(A_samples, B_samples)
    
  }
  
  list(prob = prob, 
       min.lift = ans,
       test_samples = test_samples,
       control_samples = control_samples)
  
}

get.prob <- function(A_samples, B_samples, percent_lift = 0) {
  
  prob <- mean((100 * (A_samples - B_samples) / B_samples > percent_lift))
  
  prob
  
}

