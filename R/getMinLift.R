### Gives P(A > B) by a certain lift
### Set priors for more informative predictions!

getMinLift <- function(propTest, probability = .95, maxIter = 1000, threshold = .0001) {
  
  prop_test <- propTest$inputs$clicks_test / propTest$inputs$views_test
  prop_control <- propTest$inputs$clicks_control / propTest$inputs$views_control
  
  p <- propTest$prob
  
  true_delta <- (prop_test / prop_control - 1) * 100
  
  upper <- max(true_delta, -1 * true_delta)
  lower <- -1 * upper
  attempts <- 0
  
  if(class(propTest) == 'bayesPropTest') {
    
    g <- 'get.prob(propTest$test_samples, propTest$control_samples, percent_lift = step)'
    
  }
  
  if(class(propTest) == 'bayesPropTestClosed') {
    
    g <- 'bayesPropTestClosed(propTest$inputs$clicks_test,
                             propTest$inputs$views_test,
                             propTest$inputs$clicks_control,
                             propTest$inputs$views_control,
                             percent_lift = step)$prob'
    
  }
  
  while(abs(p - .95) > threshold) {
    
    step <- mean(c(lower, upper))
    
    p <- eval(parse(text = g))
    
    if(p > .95) lower <- step
    
    if(p < .95) upper <- step
    
    if(attempts == maxIter) {
      
      step <- step
      break
      
    }
    
    attempts <- attempts + 1
    
  }
  
  result <- list(test = deparse(substitute(propTest)),
                 probability = probability,
                 maxIter = maxIter,
                 threshold = threshold,
                 minLift = step,
                 actualProb = p)
  
  class(result) <- 'minLift'
  
  return(result)
  
  #quantile((AB1$test_samples - AB1$control_samples) / AB1$control_samples, .05)
  
  
}


