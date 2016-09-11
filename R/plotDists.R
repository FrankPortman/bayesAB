#' Plot the PDF of the Log Normal distribution.
#' 
#' @param mu \eqn{\mu} parameter of the Log Normal distribution.
#' @param sigma \eqn{\sigma} parameter of the Log Normal distribution.
#' @param area  control x-axis limits (default is set to view 99\% of the area under the density curve)
#' @return The PDF of Log Normal(\eqn{\mu}, \eqn{\sigma^2}).
#' @examples
#' plotLogNormal(1, 1)
#' plotLogNormal(2, 5)
#' @export
plotLogNormal <- function(mu, sigma, area = .99) {
  
  if(area <= 0 | area >= 1) stop('area must be in (0, 1)')
  
  support <- seq(.01, qlnorm(area, meanlog = mu, sdlog = sigma, .01))
  hseq <- dlnorm(support, meanlog = mu, sdlog = sigma)
  
  plotDist(support, hseq, "Log Normal", c('mu' = mu, 'sigma' = sigma))
  
}

#' Plot the PDF of the Poisson distribution.
#' 
#' @param lambda \eqn{\lambda} parameter of the Poisson distribution.
#' @param area  control x-axis limits (default is set to view 99\% of the area under the density curve)
#' @return The PDF of Poisson(\eqn{\lambda}).
#' @examples
#' plotPoisson(1)
#' plotPoisson(5)
#' @export
plotPoisson <- function(lambda, area = .99) {
  
  if(area <= 0 | area >= 1) stop('area must be in (0, 1)')
  
  support <- 0:(qpois(area, lambda))
  hseq <- dpois(support, lambda)
  
  plotDist(support, hseq, "Poisson", c('lambda' = lambda))
  
}

#' Plot the PDF of the Pareto distribution.
#' 
#' @param xm xm parameter of the Pareto distribution.
#' @param alpha alpha parameter of the Pareto distribution. 
#' @param area  control x-axis limits (default is set to view 65\% of the area under the density curve. Be careful tweaking this for Pareto.)
#' @return The PDF of Pareto(xm, alpha).
#' @examples
#' plotPareto(1, 1)
#' plotPareto(5, 3)
#' @export
plotPareto <- function(xm, alpha, area = .65) {
  
  if(area <= 0 | area >= 1) stop('area must be in (0, 1)')
  
  support <- seq((xm - 3), qpareto(area, xm, alpha), .01) 
  hseq <- dpareto(support, xm, alpha)
  
  plotDist(support, hseq, "Pareto", c('xm' = xm, 'alpha' = alpha))
  
}

#' Plot the PDF of the Normal distribution.
#' 
#' @param mu \eqn{\mu} parameter of the Normal distribution.
#' @param s_sq \eqn{\sigma^2} parameter of the Normal distribution.
#' @return The PDF of Normal(\eqn{\mu}, \eqn{\sigma^2}).
#' @examples
#' plotNormal(1, 1)
#' plotNormal(2, 5)
#' @export
plotNormal <- function(mu, s_sq) {
  
  support <- seq(mu - s_sq * 5, mu + s_sq * 5, .001)
  hseq <- dnorm(support, mu, s_sq)
  
  plotDist(support, hseq, "Normal", c('mu' = mu, 's_sq' = s_sq))
  
}

#' Plot the PDF of the Gamma distribution.
#' 
#' @param shape shape (\eqn{\alpha}) parameter of the Gamma distribution.
#' @param rate rate (\eqn{\beta}) parameter of the Gamma distribution.
#' @param area  control x-axis limits (default is set to view 99\% of the area under the density curve)
#' @return The PDF of Gamma(shape, rate).
#' @details Note: We use the shape/rate parametrization of Gamma. See https://en.wikipedia.org/wiki/Gamma_distribution for details.
#' @examples
#' plotGamma(1, 1)
#' plotGamma(2, 5)
#' @export
plotGamma <- function(shape, rate, area = .99) {
  
  if(area <= 0 | area >= 1) stop('area must be in (0, 1)')
  
  support <- seq(.01, qgamma(area, shape = shape, rate = rate), .01)
  hseq <- dgamma(support, shape = shape, rate = rate)
  
  plotDist(support, hseq, "Gamma", c('shape' = shape, 'rate' = rate))
  
}

#' Plot the PDF of the Beta distribution.
#' 
#' @param alpha \eqn{\alpha} parameter of the Beta distribution.
#' @param beta \eqn{\beta} parameter of the Beta distribution.
#' @return The PDF of Beta(\eqn{\alpha}, \eqn{\beta}).
#' @examples
#' plotBeta(1, 1)
#' plotBeta(2, 5)
#' @export
plotBeta <- function(alpha, beta) {
  
  support <- seq(0, 1, .001)
  hseq <- dbeta(support, alpha, beta)
  
  plotDist(support, hseq, "Beta", c('alpha' = alpha, 'beta' = beta))
  
}

#' Plot the PDF of the Inverse Gamma distribution.
#' 
#' @param shape shape parameter of the Inverse Gamma distribution.
#' @param scale scale parameter of the Inverse Gamma distribution.
#' @param area  control x-axis limits (default is set to view 99\% of the area under the density curve)
#' @return The PDF of InvGamma(shape, scale).
#' @examples
#' plotInvGamma(2, 4)
#' plotInvGamma(1, 17)
#' @export
plotInvGamma <- function(shape, scale, area = .99) {
  
  if(area <= 0 | area >= 1) stop('area must be in (0, 1)')
  
  support <- seq(.01, qinvgamma(area, shape, scale), .01)
  hseq <- dinvgamma(support, shape, scale)
  
  plotDist(support, hseq, "InvGamma", c('shape' = shape, 'scale' = scale))
  
}

qinvgamma <- function(area, shape, scale) {
  
  if(shape > 0 & scale > 0 & all(area > 0) & all(area < 1)) {
    if((1 - area) <= .Machine$double.eps) {
      out <- Inf
    }
    else {
      out <- 1 / qgamma(1 - area, shape, scale)
    }
  }
  else stop('qinvgamma: invalid parameters\n')
  return(out)
}

dinvgamma <- function(x, shape, scale) {

    if (shape <= 0 | scale <= 0) {
      stop("Shape or scale parameter negative in dinvgamma().\n")
    }

    alpha <- shape
    beta <- scale
    log.density <- alpha * log(beta) - lgamma(alpha) - (alpha + 1) * log(x) - (beta / x)

    return(exp(log.density))

}

plotDist <- function(support, hseq, dist, params) {
  
  discretes <- c('Poisson')
  
  ribbon_or_bar <- ggplot2::geom_ribbon(ymin = 0, ymax = hseq, size = 2, color = I("lightblue"), fill = "lightgreen", alpha = .25)
  
  if(dist %in% discretes) ribbon_or_bar <- ggplot2::geom_bar(stat = "identity", color = I("lightblue"), fill = "lightgreen", alpha = .25, size = 2)
    
  paramList <- sapply(1:length(params), function(x) paste(names(params)[x], params[x], sep = " = ", collapse = ""))
  paramList <- paste0(paramList, collapse = ", ")
  
  p <- ggplot2::qplot(x = support, y = hseq, geom = "line") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('PDF') +
    ggplot2::ggtitle(paste(
      dist,
      'Probability Density Function for Parameters: ',
      paramList,
      collapse = "")) +
    ribbon_or_bar +
    ggplot2::theme_minimal()
  
  print(p)
  
}
