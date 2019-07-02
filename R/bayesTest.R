#' Fit a Bayesian model to A/B test data.
#'
#' @description This function fits a Bayesian model to your A/B testing sample data. See \bold{Details} for more information on usage.
#'
#' @param A_data Vector of collected samples from recipe A
#' @param B_data Vector of collected samples from recipe B
#' @param priors Named vector or named list providing priors as required by the specified distribution:
#' \itemize{
#'           \item     For 'bernoulli' distribution \code{list("alpha" = val1, "beta" = val2)}
#'           \item     For 'normal' distribution \code{c("mu" = val1, "lambda" = val2, "alpha" = val3, "beta" = val4)}
#'           \item     For 'lognormal' distribution \code{c("mu" = val1, "lambda" = val2, "alpha" = val3, "beta" = val4)}
#'           \item     For 'poisson' distribution \code{c("shape" = val1, "rate" = val2)}
#'           \item     For 'exponential' distribution \code{list("shape" = val1, "rate" = val2)}
#'           \item     For 'uniform' distribution \code{c("xm" = val1, "alpha" = val2)}
#'           \item     For 'bernoulliC' distribution: same prior definitions as 'bernoulli'
#'           \item     For 'poissonC' distribution: same prior definitions as 'poisson'
#'           }
#'
#'           See \link{plotDistributions} or the \emph{Note} section of this help document for more info.
#' @param n_samples Number of posterior samples to draw. Should be large enough for the distribution to converge. 1e5 is a good rule of thumb.
#' Not used for closed form tests.
#' @param distribution Distribution of underlying A/B test data.
#' @return A \code{bayesTest} object of the appropriate distribution class.
#'
#' @details \code{bayesTest} is the main driver function of the \bold{bayesAB} package. The input takes two vectors of data,
#' corresponding to recipe A and recipe B of an A/B test. Order does not matter, except for interpretability of the final
#' plots and intervals/point estimates. The Bayesian model for each distribution uses conjugate priors which must
#' be specified at the time of invoking the function. Currently, there are \emph{eight} supported distributions for the underlying data:
#'
#' \itemize{
#'
#' \item Bernoulli: If your data is well modeled by 1s and 0s, according to a specific probability \code{p} of a 1 occurring
#'    \itemize{\item For example, click-through-rate /conversions for a page
#'             \item Data \bold{must} be in a \{0, 1\} format where 1 corresponds to a 'success' as per the Bernoulli distribution
#'             \item Uses a conjugate \code{Beta} distribution for the parameter \bold{p} in the Bernoulli distribution
#'             \item \code{alpha} and \code{beta} must be set for a prior distribution over \bold{p}
#'             \itemize{\item alpha = 1, beta = 1 can be used as a diffuse or uniform prior}}
#'
#' \item Normal: If your data is well modeled by the normal distribution, with parameters \eqn{\mu}, \eqn{\sigma^2} controlling mean and variance
#' of the underlying distribution
#'    \itemize{\item Data \emph{can} be negative if it makes sense for your experiment
#'             \item Uses a conjugate \code{NormalInverseGamma} distribution for the parameters \bold{\eqn{\mu}} and \bold{\eqn{\sigma^2}} in the 
#'                   Normal Distribution.
#'             \item \code{mu}, \code{lambda}, \code{alpha}, and \code{beta} must be set for prior
#'             distributions over \bold{\eqn{\mu, \sigma^2}} in accordance with the parameters of the conjugate prior distributions:
#'             \itemize{\item \eqn{\mu, \sigma^2} ~ NormalInverseGamma(mu, lambda, alpha, beta)}
#'             \item This is a bivariate distribution (commonly used to model mean and variance of the normal distribution). 
#'                   You may want to experiment with both this distribution and the \code{plotNormal} and \code{plotInvGamma} outputs 
#'                   separately before arriving at a suitable set of priors for the Normal and LogNormal \code{bayesTest}}.
#'
#' \item LogNormal: If your data is well modeled by the log-normal distribution, with parameters \eqn{\mu}, \eqn{\sigma^2} as the \bold{parameters}
#' of the corresponding log-normal distribution (log of data is ~ N(\eqn{\mu}, \eqn{\sigma^2}))
#'    \itemize{\item Support for a log-normal distribution is strictly positive
#'             \item The Bayesian model requires same conjugate priors on \eqn{\mu}, \eqn{\sigma^2} as for the Normal Distribution priors
#'             \item Note: The \eqn{\mu} and \eqn{\sigma^2} are not the mean/variance of lognormal numbers themselves but are rather the
#'                   corresponding parameters of the lognormal distribution. Thus, posteriors for the statistics 'Mean' and 'Variance'
#'                   are returned alongside 'Mu' and 'Sig_Sq' for interpretability.}
#'
#' \item Poisson: If your data is well modeled by the Poisson distribution, with parameter \eqn{\lambda} controlling the average number of events
#' per interval.
#'    \itemize{\item For example, pageviews per session
#'             \item Data \emph{must} be strictly integral or 0.
#'             \item Uses a conjugate \code{Gamma} distribution for the parameter \bold{\eqn{\lambda}} in the Poisson Distribution
#'             \item \code{shape} and \code{rate} must be set for prior distribution over \eqn{\lambda}}
#'
#' \item Exponential: If your data is well modeled by the Exponential distribution, with parameter \eqn{\lambda} controlling the
#' rate of decay.
#'    \itemize{\item For example, time spent on a page or customers' LTV
#'             \item Data \emph{must} be strictly >= 0
#'             \item Uses a conjugate \code{Gamma} distribution for the parameter \bold{\eqn{\lambda}} in the Exponential Distribution
#'             \item \code{shape} and \code{rate} must be set for prior distribution over \eqn{\lambda}}
#'
#' \item Uniform: If your data is well modeled by the Uniform distribution, with parameter \eqn{\theta} controlling the \emph{max} value.
#'    \itemize{\item bayesAB has only implemented Uniform(0, \eqn{\theta}) forms
#'             \item For example, estimating max/total inventory size from individually numbered snapshots
#'             \item Data \emph{must} be strictly > 0
#'             \item Uses a conjugate \code{Pareto} distribution for the parameter \bold{\eqn{\theta}} in the Uniform(0, \eqn{\theta}) Distribution
#'             \item \code{xm} and \code{alpha} must be set for prior distribution over \eqn{\theta}}
#'
#' \item BernoulliC: Closed form (computational) calculation of the 'bernoulli' bayesTest. Same priors are required.
#' \item PoissonC: Closed form (computational) calculation of the 'poisson' bayesTest. Same priors are required.
#' }
#'
#' @note For 'closed form' tests, you do not get a distribution over the posterior, but simply P(A > B) for the parameter in question.
#'
#' Choosing priors correctly is very important. Please see http://fportman.com/writing/bayesab-0-dot-7-0-plus-a-primer-on-priors/ for a detailed example of choosing priors
#' within bayesAB. Here are some ways to leverage objective/diffuse (assigning equal probability to all values) priors:
#'
#' \itemize{\item \code{Beta}(1, 1)
#'          \item \code{Gamma}(eps, eps) ~ \code{Gamma}(.00005, .00005) will be effectively diffuse
#'          \item \code{InvGamma}(eps, eps) ~ \code{InvGamma}(.00005, .00005) will be effectively diffuse
#'          \item \code{Pareto}(eps, eps) ~ \code{Pareto}(.005, .005) will be effectively diffuse}
#'
#' Keep in mind that the Prior Plots for bayesTest's run with diffuse priors may not plot correctly as they will not be truncated as they
#' approach infinity. See \link{plot.bayesTest} for how to turn off the Prior Plots.
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
#' AB2 <- bayesTest(A_norm, B_norm,
#'                  priors = c('mu' = 5, 'lambda' = 1, 'alpha' = 3, 'beta' = 1), 
#'                  distribution = 'normal')
#'
#' print(AB1)
#' summary(AB1)
#' plot(AB1)
#'
#' summary(AB2)
#'
#' # Create a new variable that is the probability multiiplied
#' # by the normally distributed variable (expected value of something)
#' AB3 <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation')
#'
#' print(AB3)
#' summary(AB3)
#' plot(AB3)
#'
#' @export
bayesTest <- function(A_data,
                      B_data,
                      priors,
                      n_samples = 1e5,
                      distribution = c('bernoulli', 'normal', 'lognormal',
                                       'poisson', 'exponential', 'uniform',
                                       'bernoulliC', 'poissonC')) {
  # Coerce inputs
  distribution <- match.arg(distribution)
  data <- list(A_data = A_data, B_data = B_data)
  priors <- as.list(priors)

  # Import generic data/priors checks
  genericDataChecks <- list(
    checkNumericData,
    checkCompleteData
  )
  genericPriorChecks <- list(
      checkNumericPriors
  )

  # Import and prepare distribution specific functions
  Funcs <- getDistribution(distribution)
  priorArgs <- names(formals(Funcs$posteriors))
  priorArgs <- removeGenericArgs(priorArgs)

  ###
  # Generic checks
  ###
  funcLooper(data, genericDataChecks)
  funcLooper(priors, genericPriorChecks)

  # The following are explicit for clarity in error messages
  if(length(priorArgs) != length(priors)) {
    stop("Incorrect number of priors for supplied distribution.")
  }
  if(! all(priorArgs %in% names(priors))) {
    stop("Misnamed priors provided for supplied distribution.")
  }

  ###
  # Distribution specific checks
  ###
  funcLooper(data, Funcs$dataChecks)
  funcLooper(priors, Funcs$priorChecks)

  # Construct call for posterior
  fcall <- c(data, priors)
  if(!isClosed(distribution)) fcall <- c(fcall, n_samples = n_samples)

  posteriors <- do.call(Funcs$posteriors, fcall)

  result <- list(
    inputs = list(
      A_data = list(A = A_data),
      B_data = list(B = B_data),
      priors = priors,
      n_samples = n_samples,
      distribution = distribution
    ),
    prior = Funcs$prior,
    posteriors = posteriors
  )
  class(result) <- ifelse(isClosed(distribution), 'bayesTestClosed', 'bayesTest')
  return(result)
}
