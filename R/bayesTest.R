#' Fit a Bayesian model to A/B test data.
#' 
#' @description This function fits a Bayesian model to your A/B testing sample data. See \bold{Details} for more information on usage.
#' 
#' @param A_data Vector of collected samples from recipe A
#' @param B_data Vector of collected samples from recipe B
#' @param priors Named vector or named list providing priors as required by the specified distribution:
#' \itemize{
#'           \item     For 'bernoulli' distribution \code{list("alpha" = val1, "beta" = val2)}
#'           \item     For 'normal' distribution \code{c("m0" = val1, "k0" = val2, "s_sq0" = val3, "v0" = val4)}
#'           \item     For 'lognormal' distribution \code{list("m0" = val1, "k0" = val2, "s_sq0" = val3, "v0" = val4)}
#'           }
#' @param n_samples Number of posterior samples to draw. Should be large enough for the distribution to converge. 1e5 is a good rule of thumb.
#' @param distribution Distribution of underlying A/B test data. Should be one of \code{c('bernoulli', 'normal', 'lognormal')}
#' @return A \code{bayesTest} object of the appropriate distribution class.
#' 
#' @details \code{bayesTest} is the main driver function of the \bold{bayesAB} package. The input takes two vectors of data,
#' corresponding to recipe A and recipe B of an A/B test. Order does not matter, except for interpretability of the final
#' plots and intervals/point estimates. The Bayesian model for each distribution uses conjugate priors which must 
#' be specified at the time of invoking the function. Currently, there are three supported distributions for the underlying data:
#' 
#' \itemize{
#' 
#' \item Bernoulli: If your data is well modeled by 1s and 0s, according to a specific probability \code{p} of a 1 occuring
#'    \itemize{\item For example, click-through-rate /conversions for a page
#'             \item Data \bold{must} be in a \{0, 1\} format where 1 corresponds to a 'success' as per the Bernoulli distribution
#'             \item Uses a conjugate \code{Beta} distribution for the parameter \bold{p} in the Bernoulli distribution
#'             \item \code{alpha} and \code{beta} must be set for a prior distribution over \bold{p}
#'             \itemize{\item alpha = 1, beta = 1 can be used as a diffuse or uniform prior}}
#'             
#' \item Normal: If your datat is well modeled by the normal distribution, with parameters \eqn{\mu}, \eqn{\sigma^2} controlling mean and variance
#' of the underlying distribution
#'    \itemize{\item Data \emph{can} be negative if it makes sense for your experiment
#'             \item Uses a conjugate \code{Normal} distribution for the parameter \bold{\eqn{\mu}} in the Normal Distribution
#'             \item Uses a conjugate \code{Inverse Gamma} distribution for the parameter \bold{\eqn{\sigma^2}} in the Normal Ditribution
#'             \item \code{m0}, \code{k0}, \code{s_sq0}, and \code{v0} must be set for prior 
#'             distributions over \bold{\eqn{\mu}, \eqn{\sigma^2}} in accordance with the parameters of the conjugate prior distributions:
#'             \itemize{\item \eqn{\mu} ~ Normal(m0, k0) \item \eqn{\sigma^2} ~ InvGamma(s_sq0, v0)}}
#'             
#' \item LogNormal: If your data is well modeled by the log-normal distribution, with parameters \eqn{\mu}, \eqn{\sigma^2} as the \bold{parameters}
#' of the corresponding log-normal distribution (log of data is ~ N(\eqn{\mu}, \eqn{\sigma^2}))
#'    \itemize{\item Support for a log-normal distribution is stricly positive
#'             \item The Bayesian model requires same conjugate priors on \eqn{\mu}, \eqn{\sigma^2} as for the Normal Distribution priors}
#' 
#' }
#' 
#' #' Once a Bayesian model is fit you can print/plot/minLift/confInt.
#' 
#' Adding and multiplying to chain together distributions.
#' 
#' @examples
#' A_data <- rbinom(100, 1, .5)
#' B_data <- rbinom(100, 1, .6)
#' AB1 <- bayesTest(A_data, B_data, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
#' print(AB1)
#' summary(AB1)
#' @export 

bayesTest <- function(A_data,
                      B_data,
                      priors,
                      n_samples = 1e5,
                      distribution = c('bernoulli', 'normal', 'lognormal', 
                                       'poisson', 'exponential', 'uniform',
                                       'bernoulliC', 'poissonC')) {
  
  funs <- list("bernoulli" = bayesBernoulliTest,
               "normal" = bayesNormalTest,
               "lognormal" = bayesLogNormalTest,
               "poisson" = bayesPoissonTest,
               "exponential" = bayesExponentialTest,
               "uniform" = bayesUniformTest,
               "bernoulliC" = bayesBernoulliTestClosed,
               "poissonC" = bayesPoissonTestClosed)
  
  distribution <- match.arg(distribution)
  
  if(!distribution %in% names(funs)) stop("Did not specify a valid distribution.")
  
  fcall <- list(A_data, B_data, priors)
  if(!grepl("C", distribution)) fcall <- c(fcall, n_samples)
  
  do.call(funs[[distribution]], fcall)
  
}
