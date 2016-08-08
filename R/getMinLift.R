### Gives P(A > B) by a certain lift
### Set priors for more informative predictions!

getMinLift <- function(bayesTest, threshold = c(.9, .95), varName, eval = F) {
  
  if (is(bayesTest, 'bayesBernoulliTest')) {
    varName <- "probs"
  }
  
  if (missing(varName)) {
    stop("Must supply varName. e.g. 'mus', 'sig_sqs' or 'means'")
  }
  
  varName <- tolower(varName)
  varNameTest <- paste0("A_",varName)
  
  if (varNameTest %in% names(bayesTest$posteriors)) {
    test_samples <- bayesTest$posteriors[[paste0("A_",varName)]]
    control_samples <- bayesTest$posteriors[[paste0("B_",varName)]]
  } else {
    stop(paste0("Cannot find variable name '", varNameTest, "' within posteriors."))
  }
  
  vals <- quantile((test_samples - control_samples) / control_samples, 1 - threshold)
  
  if(eval) {
    
    if (is(bayesTest,'bayesNormalTest')) {
      distribution = "normal"
    } else if (is(bayesTest, 'bayesLogNormalTest')) {
      distribution = "lognormal"
    } else if (is(bayesTest, 'bayesBernoulliTest')) {
      distribution = "bernoulli"
    } else {
      stop("Could not find correct distribution for bayesTest")
    }
    
    evalThreshold <- function(single_threshold) {
      tmpEval <- bayesTest(bayesTest$inputs$A_data,
                             bayesTest$inputs$B_data,
                             bayesTest$inputs$priors,
                             1 - single_threshold,
                             n_samples = bayesTest$inputs$n_samples,
                             distribution)
      
      test_samples <- tmpEval$posteriors[[paste0("A_",varName)]]
      control_samples <- tmpEval$posteriors[[paste0("B_",varName)]]
      
      quantile((test_samples - control_samples) / 
                 control_samples, 1 - single_threshold)
      
    }
    
    vals <- sapply(threshold, evalThreshold)
    
  }
  
  result <- list(test = deparse(substitute(bayesTest)),
                 probability = threshold,
                 minLift = as.numeric(vals),
                 testVars = c(paste0("A_",varName), paste0("B_",varName)))
  
  class(result) <- 'minLift'
  
  return(result)
  
}

getCredibleInterval <- function(bayesTest, interval_start = .5, interval_end = .95) {
  
  
  
}

getMinLiftDeprecated <- function(propTest, probability = .95, maxIter = 1000, threshold = .0001) {
  
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


