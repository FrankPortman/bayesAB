#' Plot bayesTest objects
#'
#' @description Plot method for objects of class "bayesTest".
#'
#' @param x an object of class "bayesTest"
#' @param percentLift a vector of length(x$posteriors). Each entry corresponds to the percent lift ((A - B) / B) to plot for for
#'        the respective posterior in x. Note this is on a 'point' scale. percentLift = 5 implies you want to test for a 5\% lift.
#' @param priors logical indicating whether prior plots should be generated.
#' @param posteriors logical indicating whether posterior plots should be generated.
#' @param samples logical indicating whether sample plots should be generated.
#' @param ... graphics parameters to be passed to the plotting routines. (For example \code{p}, in prior plots)
#'
#' @note You can either directly plot a bayesTest object (in which case it will plot interactively), or you can save the plot
#' object to a variable and extract what you need separately. If extracted, you can treat it like any \code{ggplot2} object and
#' modify it accordingly.
#' 
#' Plots are slightly truncated on the extremes to solve the general case of potentially long tail-ends.
#'
#' @examples
#' A_pois <- rpois(100, 5)
#' B_pois <- rpois(100, 4.7)
#'
#' AB1 <- bayesTest(A_pois, B_pois, priors = c('shape' = 25, 'rate' = 5), distribution = 'poisson')
#'
#' plot(AB1)
#' plot(AB1, percentLift = 5)
#'
#' p <- plot(AB1)
#'
#' p$posteriors$Lambda
#' \dontrun{p$posteriors$Lambda + ggtitle('yolo') # modify ggplot2 object directly}
#'
#' @export
plot.bayesTest <- function(x,
                           percentLift = rep(0, length(x$posteriors)),
                           priors = TRUE,
                           posteriors = TRUE,
                           samples = TRUE,
                           ...) {

  if(length(x$posteriors) != length(percentLift)) stop("Must supply a 'percentLift' for every parameter with a posterior distribution.")
  if(!any(priors, posteriors, samples)) stop("Must specifiy at least one plot to make.")

  pri <- post <- samp <- list() # Initialize empty list

  if(priors) pri <- plotPriors(x)
  if(posteriors) post <- plotPosteriors(x)
  if(samples) samp <- plotSamples(x, percentLift)

  out <- list(
    priors = pri,
    posteriors = post,
    samples = samp
  )

  class(out) <- "plotBayesTest"

  return(out)

}

#' @export
plot.bayesTestClosed <- function(x, ...) {
  stop("Can't plot 'closed form' bayesTest.")
}

#' @export
print.plotBayesTest <- function(x, ...) {

  oldPar <- par()$ask
  par(ask = TRUE)

  plots <- unlist(x, recursive = FALSE)

  for(p in plots) print(p)

  par(ask = oldPar)

}

#' @export
print.bayesTest <- function(x, ...) {

  cat('--------------------------------------------\n')
  cat("Distribution used: ")
  cat(x$inputs$distribution, '\n')

  cat('--------------------------------------------\n')
  cat('Using data with the following properties: \n')

  summ_outA <- sapply(x$inputs$A_data, summary)
  summ_outB <- sapply(x$inputs$B_data, summary)
  print(cbind(A_data = summ_outA, B_data = summ_outB))

  cat('--------------------------------------------\n')
  cat('Conjugate Prior Distribution: ')
  cat(x$prior, '\n')
  cat('Conjugate Prior Parameters: \n')
  print(x$inputs$priors)

  cat('--------------------------------------------\n')
  cat('Calculated posteriors for the following parameters: \n')
  cat(paste0(names(x$posteriors), collapse = ", "), '\n')

  cat('--------------------------------------------\n')
  cat('Monte Carlo samples generated per posterior: \n')
  print(x$inputs$n_samples)

}

#' Summarize bayesTest objects
#'
#' @description Summary method for objects of class "bayesTest".
#'
#' @param object an object of class "bayesTest"
#' @param percentLift a vector of length(x$posteriors). Each entry corresponds to the percent lift ((A - B) / B) to summarize for for
#'        the respective posterior in x. Note this is on a 'point' scale. percentLift = 5 implies you want to test for a 5\% lift.
#' @param credInt a vector of length(x$posteriors). Each entry corresponds to the width of credible interval of (A - B) / B to calculate for
#'        the respective posterior in x. Also on a 'point' scale.
#' @param ... additional arguments affecting the summary produced.
#' @return A \code{summaryBayesTest} object which contains summaries of the Posterior distributions, direct probabilities that A > B (by
#' \code{percentLift}), credible intervals on (A - B) / B, and the Posterior Expected Loss on all estimated parameters.
#'
#' @note The Posterior Expected Loss (https://en.wikipedia.org/wiki/Bayes_estimator) is a good indicator of when to end a Bayesian
#' AB test. If the PEL is lower than the absolute delta of the minimum effect you wish to detect, the test can be reasonably be stopped.
#'
#' @examples
#' A_pois <- rpois(100, 5)
#' B_pois <- rpois(100, 4.7)
#'
#' AB1 <- bayesTest(A_pois, B_pois, priors = c('shape' = 25, 'rate' = 5), distribution = 'poisson')
#'
#' summary(AB1)
#' summary(AB1, percentLift = 10, credInt = .95)
#'
#' @export
summary.bayesTest <- function(object,
                              percentLift = rep(0, length(object$posteriors)),
                              credInt = rep(.9, length(object$posteriors)),
                              ...) {

  if(length(object$posteriors) != length(percentLift)) stop("Must supply a 'percentLift' for every parameter with a posterior distribution.")
  if(length(object$posteriors) != length(credInt)) stop("Must supply a 'credInt' for every parameter with a posterior distribution.")
  if(any(credInt <= 0) | any(credInt >= 1)) stop("Credible interval width must be in (0, 1).")

  lifts <- lapply(object$posteriors, function(x) do.call(getLift, unname(x)))
  posteriorExpectedLoss <- lapply(object$posteriors, function(x) do.call(getPostError, unname(x)))

  probability <- Map(function(x, y) getProb(x, y), lifts, percentLift)
  interval <- Map(function(x, y) getCredInt(x, y), lifts, credInt)

  posteriorSummary <- lapply(object$posteriors, function(x) lapply(x, function(y) quantile(y)))

  out <- list(
    posteriorSummary = posteriorSummary,
    probability = probability,
    interval = interval,
    posteriorExpectedLoss = posteriorExpectedLoss,
    percentLift = percentLift,
    credInt = credInt
  )

  class(out) <- 'summaryBayesTest'

  return(out)

}

#' @export
print.summaryBayesTest <- function(x, ...) {

  cat('Quantiles of posteriors for A and B:\n\n')
  print(x$posteriorSummary)

  cat('--------------------------------------------\n\n')

  cat('P(A > B) by (', paste0(x$percentLift, collapse = ", "), ')%: \n\n', sep = "")
  print(x$probability)

  cat('--------------------------------------------\n\n')

  cat('Credible Interval on (A - B) / B for interval length(s) (', paste0(x$credInt, collapse = ", "), ') : \n\n', sep = "")
  print(x$interval)

  cat('--------------------------------------------\n\n')

  cat('Posterior Expected Loss for choosing B over A:\n\n')
  print(x$posteriorExpectedLoss)

}

#' @export
print.bayesTestClosed <- print.bayesTest

#' @export
summary.bayesTestClosed <- function(object, ...) {
  out <- list(probability = object$posteriors[1])
  class(out) <- 'summaryBayesTestClosed'
  return(out)
}

#' @export
print.summaryBayesTestClosed <- function(x, ...) {
  cat('P(A > B):\n')
  print(x$probability)
}

#' @export
print.bayesBandit <- function(x, ...) {
  x$getUpdates()
}

#' Concatenate bayesTest objects
#'
#' @description Concatenate method for objects of class "bayesTest".
#'
#' @param ... \code{bayesTest} objects
#' @param errorCheck check that objects can be concatenated? Setting this to \code{FALSE} can have unintended
#'                   effects if the \code{bayesTest} objects have different params but can have some speedups
#'                   if you are sure the objects are of the same type.
#' @return A \code{bayesTest} object with concatenated posteriors.
#'
#' @examples
#' A_pois <- rpois(100, 5)
#' B_pois <- rpois(100, 4.7)
#'
#' AB1 <- bayesTest(A_pois, B_pois, priors = c('shape' = 25, 'rate' = 5), distribution = 'poisson')
#' AB2 <- bayesTest(A_pois, B_pois, priors = c('shape' = 25, 'rate' = 5), distribution = 'poisson')
#' c(AB1, AB2)
#'
#' @export
c.bayesTest <- function(..., errorCheck = TRUE) {

  tests <- list(...)

  ## Check for mismatches in inputs
  if(errorCheck) checkInputsEqual(tests)

  result <- list()

  result$inputs <- tests[[1]]$inputs
  result$inputs$n_samples <- sum(sapply(tests, function(x) x$inputs$n_samples))

  for(posterior in names(tests[[1]]$posteriors)) {
    posts <- lapply(tests, function(test) test$posteriors[[posterior]])
    As <- do.call(c, lapply(posts, function(e) e$A))
    Bs <- do.call(c, lapply(posts, function(e) e$B))
    result$posteriors[[posterior]] <- list(A = As, B = Bs)
  }

  class(result) <- 'bayesTest'

  return(result)

}

checkInputsEqual <- function(tests) {
  fields <- c('A_data', 'B_data', 'priors', 'distribution')
  checks <- c()

  for(field in fields) {
    extract <- lapply(tests, function(test) test$inputs[[field]])
    checks[field] <- length(unique(extract)) == 1
  }

  failures <- names(checks[!checks])

  if(length(failures) >= 1) {
    errMsg <- paste0("Unable to concatenate. Mismatches in (",
                     paste0(failures, collapse = ", "),
                     "). All inputs must be the same (except n_samples).",
                     collapse = "")
    stop(errMsg)
  }
}
