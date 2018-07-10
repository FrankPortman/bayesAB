#' Combine two \code{bayesAB} objects given a binary function.
#'
#' @description Combine two (or any number, in succession) \code{bayesTest} objects into a new arbitrary posterior distribution.
#'              The resulting object is of the same class.
#'
#' @param bT1 a bayesTest object
#' @param bT2 a bayesTest object
#' @param f a binary function (f(x, y)) used to combine posteriors from bT1 to bT2
#' @param params a character vector of length 2, corresponding to names of the posterior parameters you want to combine;
#'               defaults to first posterior parameter if not supplied
#' @param newName a string indicating the name of the new 'posterior' in the resulting object;
#'                defaults to string representation of f(params[1], params[2])
#' @return a \code{bayesTest} object with the newly combined posterior samples.
#'
#' @note The generics `+.bayesTest`, `*.bayesTest`, `-.bayesTest`, and `/.bayesTest` are shorthand for
#'       combine(f = `+`), combine(f = `*`), combine(f = `-`), and combine(f = `/`).
#'
#' @seealso \code{\link{grab}}
#'
#' @examples
#' A_binom <- rbinom(100, 1, .5)
#' B_binom <- rbinom(100, 1, .6)
#'
#' A_norm <- rnorm(100, 6, 1.5)
#' B_norm <- rnorm(100, 5, 2.5)
#'
#' AB1 <- bayesTest(A_binom, B_binom,
#'                  priors = c('alpha' = 1, 'beta' = 1),
#'                  distribution = 'bernoulli')
#'
#' AB2 <- bayesTest(A_norm, B_norm,
#'                  priors = c('mu' = 5, 'lambda' = 1, 'alpha' = 3, 'beta' = 1),
#'                  distribution = 'normal')
#'
#' AB3 <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation')
#' # Equivalent to
#' AB3 <- AB1 * grab(AB2, 'Mu')
#'
#' # To get the same posterior name as well
#' AB3 <- rename(AB3, 'Expectation')
#'
#' # Dummy example
#' weirdVariable <- (AB1 + AB2) * (AB2 / AB1)
#' weirdVariable <- rename(weirdVariable, 'confusingParam')
#'
#' print(AB3)
#' summary(AB3)
#' plot(AB3)
#' @export
combine <- function(bT1, bT2, f = `+`, params, newName) {
  if(
    any(
      isClosed(bT1$inputs$distribution),
      isClosed(bT2$inputs$distribution)
    )
  ) stop("Can't combine a 'closed' bayesTest.")

  if(missing(params)) {
    params <- c(NA, NA)
    params[1] <- names(bT1$posteriors)[1]
    params[2] <- names(bT2$posteriors)[1]
  }

  if(missing(newName)) {
    input <- paste0(params, collapse = ", ")
    input <- paste0('(', input, ')')
    newName <- paste0(toString(substitute(f)), input)
  }

  if(length(params) != 2) stop('You must specify only (2) params - one for the first test and one for the second test.')

  if(!((params[1] %in% names(bT1$posteriors)) & (params[2] %in% names(bT2$posteriors)))) {
    stop("You have specified a `param` name that doesn't exist in the posterior of the first test and/or the second test.")
  }

  if(bT1$inputs$n_samples != bT2$inputs$n_samples) warning("n_samples not equal. Make sure `f` handles recycling appropriately.")

  A1 <- bT1$posteriors[[params[1]]]$A; A2 <- bT2$posteriors[[params[2]]]$A
  B1 <- bT1$posteriors[[params[1]]]$B; B2 <- bT2$posteriors[[params[2]]]$B

  result <- list()

  result$inputs <- list(
    A_data = c(bT1$inputs$A_data, bT2$inputs$A_data),
    B_data = c(bT1$inputs$B_data, bT2$inputs$B_data),
    priors = 'Combined distributions have no priors. Inspect each element separately for details.',
    n_samples = max(bT1$inputs$n_samples, bT2$inputs$n_samples),
    distribution = 'combined'
  )

  result$prior <- NULL
  result$posteriors[[newName]] <- list(A = f(A1, A2), B = f(B1, B2))

  class(result) <- c('bayesTest')

  return(result)

}

#' @export
`+.bayesTest` <- function(e1, e2) combine(e1, e2, f = `+`)

#' @export
`*.bayesTest` <- function(e1, e2) combine(e1, e2, f = `*`)

#' @export
`-.bayesTest` <- function(e1, e2) combine(e1, e2, f = `-`)

#' @export
`/.bayesTest` <- function(e1, e2) combine(e1, e2, f = `/`)

#' Grab the supplied posterior from a bayesTest object
#'
#' @description Grab the supplied posterior from a bayesTest object, returning another bayesTest object.
#'
#' @param bT a bayesTest object
#' @param posterior the name of a posterior in that object (string)
#'
#' @return a \code{bayesTest} object with the posterior parameter isolated
#'
#' @seealso \code{\link{combine}}
#'
#' @export
grab <- function(bT, posterior) {
  if(! posterior %in% names(bT$posteriors)) stop("That posterior doesn't exist in the input bayesTest.")
  result <- list(
    prior = bT$prior,
    inputs = bT$inputs,
    posteriors = bT$posteriors[posterior]
  )

  class(result) <- 'bayesTest'
  return(result)
}

#' Rename the posterior for a bayesTest object
#'
#' @description Rename the posterior param for a bayesTest object.
#'
#' @param bT a bayesTest object
#' @param newName the new name you want for the posterior param (string)
#'
#' @return a \code{bayesTest} object with the posterior parameter renamed
#'
#' @seealso \code{\link{combine}}
#'
#' @export
rename <- function(bT, newName) {
  if(length(bT$posteriors) != 1) stop('Can only rename bayesTests with one posterior.')
  names(bT$posteriors) <- newName
  return(bT)
}
