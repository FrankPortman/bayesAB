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
  
  p <- qplot(x = seq, y = hseq, geom = "line") +
          xlab(NULL) + 
          ylab('PDF') + 
          ggtitle(paste('Probability Density Function for Parameters: alpha = ', alpha, ', beta = ', beta, sep = ''))
  
  
  print(p)
  
}
