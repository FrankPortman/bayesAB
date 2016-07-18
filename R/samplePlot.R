samplePlot <- function(A, B, percent_lift, name) {
  
  diff <- (A - B) / B
  diff <- data.frame(diff = diff, cutoff = diff < cutoff)
  cutoff <- percent_lift / 100
  
  prop <- 1 - sum(diff$cutoff) / nrow(diff)
  prop <- round(prop * 100, digits = 1)
  
  p <- ggplot2::qplot(diff, data = diff, fill = cutoff, binwidth = diff(range(diff)) / 250) + 
    ggplot2::geom_vline(xintercept = cutoff)
  
  m <- max(ggplot2::ggplot_build(p)$panel$ranges[[1]]$y.range)
  
  xpos <- mean(diff$diff[diff$cutoff == F])
  if(is.nan(xpos)) xpos <- cutoff + 5
  
  p <- p + ggplot2::annotate('text', x = xpos, y = m / 3, label = paste(prop, '%', sep = "")) +
    ggplot2::xlab('(Test - Control) / Control') +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('Histogram of (Test - Control) / Control Samples : ',
                            name,
                            collapse = "")) +
    ggplot2::theme(legend.position = "none")
  
  print(p) 
  
}