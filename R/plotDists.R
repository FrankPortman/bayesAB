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
