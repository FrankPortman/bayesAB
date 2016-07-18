#' Plot the PDF of the Normal distribution.
#' 
#' @param mu μ parameter of the Normal distribution.
#' @param s_sq σ² parameter of the Normal distribution.
#' @return The PDF of Normal(μ, σ²).
#' @examples
#' plotNormal(1, 1)
#' plotNormal(2, 5)


plotNormal <- function(mu, s_sq) {
  
  support <- seq(mu - s_sq * 5, mu + s_sq * 5, .001)
  hseq <- dnorm(support, mu, s_sq)
  
  p <- ggplot2::qplot(x = support, y = hseq, geom = "line") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('PDF') +
    ggplot2::ggtitle(paste('Probability Density Function for Parameters: mu = ', mu, ', s_sq = ', s_sq, sep = ""))
  
  print(p)
  
}

#' Plot the PDF of the Beta distribution.
#' 
#' @param alpha α parameter of the Beta distribution.
#' @param beta β parameter of the Beta distribution.
#' @return The PDF of Beta(α, β).
#' @examples
#' plotBeta(1, 1)
#' plotBeta(2, 5)

plotBeta <- function(alpha, beta) {
  
  seq <- seq(0, 1, .001)
  hseq <- dbeta(seq, alpha, beta)
  
  p <- ggplot2::qplot(x = seq, y = hseq, geom = "line") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, .1)) +
    ggplot2::xlab(NULL) + 
    ggplot2::ylab('PDF') + 
    ggplot2::ggtitle(paste('Probability Density Function for Parameters: alpha = ', alpha, ', beta = ', beta, sep = ''))
  
  
  print(p)
  
}

#' Plot the PDF of the Inverse Gamma distribution.
#' 
#' @param shape shape parameter of the Inverse Gamma distribution.
#' @param scale scale parameter of the Inverse Gamma distribution.
#' @param p  control x-axis limits (default is set to view 95\% of the area under the density curve)
#' @return The PDF of InvGamma(shape, scale).
#' @examples
#' plotInvGamma(2, 4)
#' plotInvGamma(1, 17)

plotInvGamma <- function(shape, scale, p = .95) {
  
  if(p <= 0 | p >= 1) stop('p must be in (0, 1)')
  
  support <- seq(0, qinvgamma(p, shape, scale), .01)
  hseq <- dinvgamma(support, shape, scale)
  
  p <- ggplot2::qplot(x = support, y = hseq, geom = "line") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('PDF') +
    ggplot2::ggtitle(paste('Probability Density Function for Parameters: shape = ', shape, ', scale = ', scale, sep = ""))
  
  print(p)
  
}

qinvgamma <- function(p, shape, scale) {
  
  if(shape > 0 & scale > 0 & all(p > 0) & all(p < 1)) {
    if((1 - p) <= .Machine$double.eps) {
      out <- Inf
    }
    else {
      out <- 1 / qgamma(1 - p, shape, scale)
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
