#' Plot distributions to explore data and/or choose priors.
#'
#' @docType data
#' @name plotDistributions
#'
#' @description These are helper functions to help explore data and/or choose priors. Click further for more details.
#'
#' \itemize{
#' \item \link{plotBeta}
#' \item \link{plotGamma}
#' \item \link{plotInvGamma}
#' \item \link{plotLogNormal}
#' \item \link{plotNormal}
#' \item \link{plotPareto}
#' \item \link{plotPoisson}
#' }
#'
#' @note Choosing priors correctly is very important. Please see http://fportman.com/writing/bayesab-0-dot-7-0-plus-a-primer-on-priors/ for a detailed example of choosing priors
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
#' plot{...} functions are generated programmatically so the function calls in
#' their \code{body} will be substituted directly
NULL

plotDist_ <- function(support, hseq, dist, params) {

  discretes <- c('Poisson')

  ribbon_or_bar <- ggplot2::geom_ribbon(ggplot2::aes(ymax = .data$hseq),
                                        ymin = 0,
                                        size = 2,
                                        color = I("lightblue"),
                                        fill = "lightgreen",
                                        alpha = .25)

  if(dist %in% discretes) {
    ribbon_or_bar <- ggplot2::geom_col(size = 2,
                                       color = I("lightblue"),
                                       fill = "lightgreen",
                                       alpha = .25)
    notEmpty <- hseq != 0
    support <- support[notEmpty]
    hseq <- hseq[notEmpty]
  }

  # Done after the if statement because we have to delete some stuff for discrete case
  data <- data.frame(support, hseq)

  paramList <- sapply(names(params), function(p) paste(p, params[p], sep = " = ", collapse = ""), USE.NAMES = FALSE)
  paramList <- paste0(paramList, collapse = ", ")

  p <- ggplot2::ggplot(data, ggplot2::aes(support, hseq)) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('PDF') +
    ggplot2::ggtitle(paste0(
      dist,
      ' Probability Density Function for Parameters: ',
      paramList)) +
    ribbon_or_bar +
    theme_bayesAB()

  p

}

alist2 <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  res
}

plotDist <- function(dist, name, args) {
  makeDistFunc <- function(t) eval(parse(text = paste0(t, dist)))
  qDist <- makeDistFunc('q')
  dDist <- makeDistFunc('d')

  distArgs <- sapply(args, as.name, USE.NAMES = FALSE)

  out <- function() {}

  body(out) <- substitute({
    supportArgs <- list(c(0, 0.01, 0.99, 1))
    support <- do.call(qDist, c(supportArgs, distArgs))

    support <- range(support[is.finite(support)])
    support <- seq(min(support), max(support), diff(support) / 1000)
    support <- c(support, round(support)) # Include integers for discrete
    support <- unique(support)

    rangeArgs <- list(support)
    hseq <- suppressWarnings(do.call(dDist, c(rangeArgs, distArgs)))

    plotDist_(support, hseq, name, match.call()[-1])
  })

  formals(out) <- alist2(args)
  out
}

#' Plot the PDF of the Log Normal distribution.
#'
#' @param mu \eqn{\mu} parameter of the Log Normal distribution.
#' @param sigma \eqn{\sigma} parameter of the Log Normal distribution.
#' @return The PDF of Log Normal(\eqn{\mu}, \eqn{\sigma^2}).
#' @note The output can be treated like any \code{ggplot2} object and modified accordingly.
#' @examples
#' plotLogNormal(1, 1)
#' plotLogNormal(2, .5)
#' \dontrun{plotLogNormal(2, .5) + ggtitle('I hate the default title!')}
#' @export
plotLogNormal <- plotDist('lnorm', 'Log Normal', c('mu', 'sigma'))

#' Plot the PDF of the Poisson distribution.
#'
#' @param lambda \eqn{\lambda} parameter of the Poisson distribution.
#' @return The PDF of Poisson(\eqn{\lambda}).
#' @note The output can be treated like any \code{ggplot2} object and modified accordingly.
#' @examples
#' plotPoisson(1)
#' plotPoisson(5)
#' \dontrun{plotPoisson(5) + ggtitle('I hate the default title!')}
#' @export
plotPoisson <- plotDist('pois', 'Poisson', c('lambda'))

#' Plot the PDF of the Pareto distribution.
#'
#' @param xm xm parameter of the Pareto distribution.
#' @param alpha alpha parameter of the Pareto distribution.
#' @return The PDF of Pareto(xm, alpha).
#' @note The output can be treated like any \code{ggplot2} object and modified accordingly.
#' @examples
#' plotPareto(1, 1)
#' plotPareto(5, 3)
#' \dontrun{plotPareto(5, 3) + ggtitle('I hate the default title!')}
#' @export
plotPareto <- plotDist('pareto', 'Pareto', c('xm', 'alpha'))

#' Plot the PDF of the Normal distribution.
#'
#' @param mu \eqn{\mu} parameter of the Normal distribution.
#' @param sd \eqn{\sigma} parameter of the Normal distribution.
#' @return The PDF of Normal(\eqn{\mu}, \eqn{\sigma^2}).
#' @note The output can be treated like any \code{ggplot2} object and modified accordingly.
#' @examples
#' plotNormal(1, 1)
#' plotNormal(2, 5)
#' \dontrun{plotNormal(2, 5) + ggtitle('I hate the default title!')}
#' @export
plotNormal <- plotDist('norm', 'Normal', c('mu', 'sd'))

#' Plot the PDF of the Gamma distribution.
#'
#' @param shape shape (\eqn{\alpha}) parameter of the Gamma distribution.
#' @param rate rate (\eqn{\beta}) parameter of the Gamma distribution.
#' @return The PDF of Gamma(shape, rate).
#' @note The output can be treated like any \code{ggplot2} object and modified accordingly.
#' @details Note: We use the shape/rate parametrization of Gamma. See https://en.wikipedia.org/wiki/Gamma_distribution for details.
#' @examples
#' plotGamma(1, 1)
#' plotGamma(2, 5)
#' \dontrun{plotGamma(2, 5) + ggtitle('I hate the default title!')}
#' @export
plotGamma <- plotDist('gamma', 'Gamma', c('shape', 'rate'))

#' Plot the PDF of the Beta distribution.
#'
#' @param alpha \eqn{\alpha} parameter of the Beta distribution.
#' @param beta \eqn{\beta} parameter of the Beta distribution.
#' @return The PDF of Beta(\eqn{\alpha}, \eqn{\beta}).
#' @note The output can be treated like any \code{ggplot2} object and modified accordingly.
#' @examples
#' plotBeta(1, 1)
#' plotBeta(2, 5)
#' \dontrun{plotBeta(2, 5) + ggtitle('I hate the default title!')}
#' @export
plotBeta <- plotDist('beta', 'Beta', c('alpha', 'beta'))

#' Plot the PDF of the Inverse Gamma distribution.
#'
#' @param shape shape parameter of the Inverse Gamma distribution.
#' @param scale scale parameter of the Inverse Gamma distribution.
#' @return The PDF of InvGamma(shape, scale).
#' @note The output can be treated like any \code{ggplot2} object and modified accordingly.
#'       Also note that the \code{scale} parameter of the Inverse Gamma distribution is
#'       analogous to the \code{beta} (or rate) parameter of the regular Gamma distribution.
#'       The \code{beta} parameter of the \link{plotNormalInvGamma} distribution is analogous
#'       to the \code{scale} parameter here.
#' @examples
#' plotInvGamma(2, 4)
#' plotInvGamma(1, 17)
#' \dontrun{plotInvGamma(1, 17) + ggtitle('I hate the default title!')}
#' @export
plotInvGamma <- plotDist('invgamma', 'Inverse Gamma', c('shape', 'scale'))

#' Plot the bivariate PDF of the Normal Inverse Gamma Distribution.
#'
#' @param mu \eqn{\mu} parameter of the Normal Inverse Gamma distribution.
#' @param lambda \eqn{\lambda} parameter of the Normal Inverse Gamma distribution.
#' @param alpha \eqn{\alpha} parameter of the Normal Inverse Gamma distribution.
#' @param beta \eqn{\beta} parameter of the Normal Inverse Gamma distribution.
#' @return The PDF of NormalInverseGamma(mu, lambda, alpha, beta)
#' @note This is a bivariate distribution (commonly used to model mean and
#'       variance of the normal distribution) and returns a 2d contour
#'       plot instead of a typical one dimensional PDF. You may want to experiment
#'       with both this distribution and the \code{plotNormal} and \code{plotInvGamma}
#'       outputs separately before arriving at a suitable set of priors for the
#'       Normal and LogNormal \code{bayesTest}.
#' @examples
#' plotNormalInvGamma(3, 1, 1, 1)
#' @export
plotNormalInvGamma <- function(mu, lambda, alpha, beta) {
  # Currently we do this in a semi-hacky way to ensure we cover the whole
  # probability field
  steps <- 500

  max_sig_sq <- qgamma(.99, alpha, beta) * lambda

  x_range <- c(mu - 5 * max_sig_sq, mu + 5 * max_sig_sq)
  sig_sq_range <- c(.001, max_sig_sq)

  x <- seq(min(x_range), max(x_range), diff(x_range) / steps)
  sig_sq <- seq(min(sig_sq_range), max(sig_sq_range), diff(sig_sq_range) / steps)

  inputs <- expand.grid(x, sig_sq)
  out <- dNormalInverseGamma(inputs$Var1, inputs$Var2, mu, lambda, alpha, beta)
  dat <- data.frame(x = inputs$Var1, sig_sq = inputs$Var2, res = out)

  p <- ggplot2::ggplot(dat, ggplot2::aes_string('x', 'sig_sq', z = 'res')) +
    ggplot2::ggtitle(paste0('Normal Inverse Gamma PDF for ',
                            paste0(c(mu, lambda, alpha, beta), collapse = ", "))) +
    ggplot2::stat_contour(ggplot2::aes_string(fill = '..level..'), geom = "polygon", bins = 10) +
    ggplot2::scale_fill_continuous(name = 'Probability Density', position = 'bottom') +
    theme_bayesAB() +
    ggplot2::theme(legend.position = 'bottom')

  p

}
