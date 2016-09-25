banditize <- function(bT, param, eps) {
  
  if(eps >= 1 | eps <= 0) stop("eps must be in (0, 1)")
  
  choices <- c('A', 'B')
  
  test <- bT
  prob_A_beats_B <- summary(test)$probability[[param]]
  
  current_winner <- 'A'
  if(prob_A_beats_B <= .5) current_winner <- 'B'
  
  setCurrentWinner <- function() {
    prob_A_beats_B <- summary(test)$probability[[param]]
    
    current_winner <<- 'A'
    if(prob_A_beats_B <= .5) current_winner <<- 'B'
  }
  
  serveRecipe <- function() {
    rand <- runif(1, 0, 1)
    if(rand <= eps) return(setdiff(choices, current_winner))
    return(current_winner)
  }
  
  setResults <- function(results) {
    if(!(all(names(results) %in% choices))) stop("`results` vector must only contain names 'A' and 'B'")
    
    As <- unname(results[names(results) == 'A'])
    Bs <- unname(results[names(results) == 'B'])
    
    test <<- bayesTest(c(test$inputs$A_data, As),
                       c(test$inputs$B_data, Bs),
                       test$inputs$priors,
                       test$inputs$n_samples,
                       test$distribution)
    
    setCurrentWinner()
  }
  
  getBayesTest <- function() {
    return(test)
  }
  
  list("serveRecipe" = serveRecipe,
       "setResults" = setResults,
       "getBayesTest" = getBayesTest)
  
}

