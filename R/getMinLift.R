### Gives P(A > B) by a certain lift
### Set priors for more informative predictions!

getMinLift <- function(bayesTest, threshold = c(.9, .95), varName, eval = F) {
  
  if (is(bayesTest,'bayesBernoulliTest')) {
    
    vals <- quantile((bayesTest$posteriors$test_samples - bayesTest$posteriors$control_samples) / bayesTest$posteriors$control_samples, 1 - threshold)
    
    if(eval) {
      
      evalBernoulli <- function(single_threshold) {
        tmpBernoulli <- bayesBernoulliTest(bayesTest$inputs$A_data,
                                           bayesTest$inputs$B_data,
                                           c("alpha" = bayesTest$inputs$alpha,
                                             "beta" = bayesTest$inputs$beta),
                                           1 - single_threshold,
                                           N_samp = bayesTest$inputs$N_samp)
        
        quantile((tmpBernoulli$posteriors$test_samples - tmpBernoulli$posteriors$control_samples) / 
                   tmpBernoulli$posteriors$control_samples, 1 - single_threshold)
        
      }
      
      vals <- sapply(threshold, evalBernoulli)
      
    }
    
  } else {
    
    if (missing(varName)) {
      stop("Must supply varName. e.g. 'mus', 'sig_sqs' or 'means'")
    }
    
    varName <- tolower(varName)
    varNameTest <- paste0("A_",varName)
    
    if (varNameTest %in% names(bayesTest$posteriors)) {
      test_samples <- bayesTest$posteriors[[paste0("A_",varName)]]
      control_samples <- bayesTest$posteriors[[paste0("B_",varName)]]
    } else if (varNameTest %in% names(bayesTest$posteriors$statistics)) {
      test_samples <- bayesTest$posteriors$statistics[[paste0("A_",varName)]]
      control_samples <- bayesTest$posteriors$statistics[[paste0("B_",varName)]]
    } else {
      stop(paste0("Cannot find variable name '", varNameTest, "' within posteriors or posteriors$statistics."))
    }
    
    vals <- quantile((test_samples - control_samples) / control_samples, 1 - threshold)
    
    if(eval) {
      if (is(bayesTest,'bayesNormalTest')) {
        
        evalNormal <- function(single_threshold) {
          tmpNormal <- bayesNormalTest(bayesTest$inputs$A_data,
                                       bayesTest$inputs$B_data,
                                       c("m0" = bayesTest$inputs$m0,
                                         "k0" = bayesTest$inputs$k0,
                                         "s_sq0" = bayesTest$inputs$s_sq0,
                                         "v0" = bayesTest$inputs$v0),
                                       1 - single_threshold,
                                       n_samples = bayesTest$inputs$n_samples)
          
          if (varNameTest %in% names(tmpNormal$posteriors)) {
            test_samples <- tmpNormal$posteriors[[paste0("A_",varName)]]
            control_samples <- tmpNormal$posteriors[[paste0("B_",varName)]]
          } else if (varNameTest %in% names(tmpNormal$posteriors$statistics)) {
            test_samples <- tmpNormal$posteriors$statistics[[paste0("A_",varName)]]
            control_samples <- tmpNormal$posteriors$statistics[[paste0("B_",varName)]]
          }
          
          quantile((test_samples - control_samples) / 
                     control_samples, 1 - single_threshold)
          
        }
        
        vals <- sapply(threshold, evalNormal)
        
      } else if(is(bayesTest, 'bayesLogNormalTest')) {
        
        evalLogNormal <- function(single_threshold) {
          tmpLogNormal <- bayesLogNormalTest(bayesTest$inputs$A_data,
                                       bayesTest$inputs$B_data,
                                       c("m0" = bayesTest$inputs$m0,
                                         "k0" = bayesTest$inputs$k0,
                                         "s_sq0" = bayesTest$inputs$s_sq0,
                                         "v0" = bayesTest$inputs$v0),
                                       1 - single_threshold,
                                       n_samples = bayesTest$inputs$n_samples)
          
          if (varNameTest %in% names(tmpLogNormal$posteriors)) {
            test_samples <- tmpLogNormal$posteriors[[paste0("A_",varName)]]
            control_samples <- tmpLogNormal$posteriors[[paste0("B_",varName)]]
          } else if (varNameTest %in% names(tmpLogNormal$posteriors$statistics)) {
            test_samples <- tmpLogNormal$posteriors$statistics[[paste0("A_",varName)]]
            control_samples <- tmpLogNormal$posteriors$statistics[[paste0("B_",varName)]]
          }
          
          quantile((test_samples - control_samples) / 
                     control_samples, 1 - single_threshold)
          
        }
        
        vals <- sapply(threshold, evalLogNormal)

      }
    }
  }
  
  return(vals)
  
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


