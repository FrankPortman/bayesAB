#' Create a multi-armed Bayesian bandit object.
#' 
#' @description Fit a multi-armed bandit object based on a bayesTest which can serve recommendations and adapt
#' to new data.
#' 
#' @param bT a bayesTest object
#' @param param which model parameter (posterior) to evaluate 
#' @param higher_is_better  is a higher value of `param` equivalent to a better choice?
#' @return A bayesBandit object.
#' 
#' @details \code{banditize} is an 'object-oriented' implementation of multi-armed bandits in \code{bayesAB}. It is useful in 
#' conjunction with a Shiny app or Plumber deployment. The object itself is mutable and can adapt/learn from new data without having to
#' re-assign the variable. 
#' 
#' Comes with 5 methods:
#' 
#' \itemize{
#'  \item \code{serveRecipe()}: serves a recipe to show your user based on samples from both posteriors.
#'  \item \code{setResults(results)}: set results for one or more recipes for one or more instances of feedback. Used to update bandit.
#'  \item \code{getBayesTest()}: returns most updated \code{bayesTest} object.
#'  \item \code{getOriginalTest()}: returns original \code{bayesTest} object without any updates.
#'  \item \code{getUpdates()}: returns a summarized version of all updates this bandit has processed.}
#' 
#' @examples
#' A_binom <- rbinom(100, 1, .5)
#' B_binom <- rbinom(100, 1, .6)
#' 
#' AB1 <- bayesTest(A_binom, B_binom, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
#' 
#' binomialBandit <- banditize(AB1)
#' binomialBandit$serveRecipe
#' binomialBandit$setResults(list('A' = c(1, 0, 1, 0, 0), 'B' = c(0, 0, 0, 0, 1)))
#' 
#' @export
banditize <- function(bT, param, higher_is_better = TRUE) {
  
  ## Re-assign bT to be able to report the OG one later
  test <- bT
  
  ## Only 2 recipes for now
  choices <- c('A', 'B')
  
  ## switch for higher_is_better
  compareFun <- ifelse(higher_is_better, which.max, which.min)
  
  ## Initialize counters and updates tracking
  updates <- 0
  A_ctr <- 0
  B_ctr <- 0
  
  serveRecipe <- function() {
    ## exploit iid samples from posterior
    idx <- sample(test$inputs$n_samples, 1)
    A_sample <- test$posteriors[[param]]$A_probs[idx]
    B_sample <- test$posteriors[[param]]$B_probs[idx]
    
    return(choices[compareFun(c(A_sample, B_sample))])
  }
  
  setResults <- function(results) {
    if(!(all(names(results) %in% choices))) stop("`results` list must only contain names 'A' and 'B'")
    if(is.null(results$A) & is.null(results$B)) stop("A and B can't both be NULL.")
    
    test <<- bayesTest(c(test$inputs$A_data, results$A),
                       c(test$inputs$B_data, results$B),
                       test$inputs$priors,
                       test$inputs$n_samples,
                       test$distribution)
    
    updates <<- updates + 1
    A_ctr <<- A_ctr + length(results$A)
    B_ctr <<- B_ctr + length(results$B)
    
    return(0)
  }
  
  getBayesTest <- function() {
    return(test)
  }
  
  getOriginalTest <- function() {
    return(bT)
  }
  
  getUpdates <- function() {
    cat(updates, "updates to this bandit.\n")
    
    summarizeUpdates <- function(vec, name) cat(name, "has had", length(vec), "additions with a mean of", mean(vec), ".\n")
    
    summarizeUpdates(tail(test$inputs$A_data, A_ctr), "A")
    summarizeUpdates(tail(test$inputs$B_data, B_ctr), "B")
  }
  
  out <- list("serveRecipe" = serveRecipe,
              "setResults" = setResults,
              "getBayesTest" = getBayesTest,
              "getOriginalTest" = getOriginalTest,
              "getUpdates" = getUpdates)
  
  class(out) <- 'bayesBandit'
  
  return(out)
  
}

#' Deploy a bayesBandit object as a JSON API.
#' 
#' @description Turn your bayesBandit object into an API and serve/update requests through HTTP.
#' 
#' @param bandit a bayesBandit object
#' @param port port to deploy on
#' @return An active \code{http} process on some port.
#' 
#' @details \code{deployBandit} turns a Bayesian bandit into a JSON API that accepts curl requests. Two of the five methods of 
#' bayesBandit classes are exposed: \code{serveRecipe} and \code{setResults}. Assuming the API is deployed on \code{localhost} this is an
#' example of how it would be hit:
#' 
#' \deqn{curl http://localhost:8000/serveRecipe}
#' 
#' \deqn{curl --data '{"A":[1, 0, 1, 1], "B":[0, 0, 0, 1]}' http://localhost:8000/setResults}
#' 
#' @examples 
#' A_binom <- rbinom(100, 1, .5)
#' B_binom <- rbinom(100, 1, .6)
#' 
#' AB1 <- bayesTest(A_binom, B_binom, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
#' 
#' binomialBandit <- banditize(AB1)
#' \dontrun{deployBandit(binomialBandit)}
#' 
#' @export
deployBandit <- function(bandit, port = 8000) {
  
  if(packageVersion("plumber") < 0.3) {
    stop("You must have the development version of `plumber` to use `deployBandit`. See `installPlumber()`.")
  }
  
  # Create a new router
  router <- plumber::plumber$new()
  
  serve <- expression(
    function(){
      bandit$serveRecipe()
    }
  )
  
  set <- expression(
    function(A = NULL, B = NULL){
      results <- list("A" = A, "B" = B)
      bandit$setResults(results)
    }
  )
  
  router$addEndpoint(verbs = "GET", path = "/serveRecipe",
                     expr = serve)
  
  router$addEndpoint(verbs = "POST", path = "/setResults",
                     expr = set)
  
  router$run(port = port)
  
}

#' Install the dev version of \code{plumber}.
#' 
#' @description Install the development version of \code{plumber} from Github in order for \code{deployBandit} to work.
#' 
#' @note This is to be deprecated once \code{plumber} >= 0.3 is on CRAN.
#' 
#' @examples 
#' \dontrun{installPlumber()}
#' 
#' @export
installPlumber <- function() {
  devtools::install_github("trestletech/plumber")
  requireNamespace("plumber")
}
