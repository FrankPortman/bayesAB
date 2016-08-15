samplePlot <- function(A, B, percent_lift, name) {
  
  diff <- (A - B) / B
  cutoff <- percent_lift / 100
  
  diff <- data.frame(diff = diff, cutoff = diff < cutoff)
  
  prop <- 1 - sum(diff$cutoff) / nrow(diff)
  prop <- round(prop * 100, digits = 1)
  
  p <- ggplot2::qplot(diff, data = diff, fill = cutoff, binwidth = diff(range(diff)) / 250) + 
    ggplot2::geom_vline(xintercept = cutoff)
  
  m <- max(ggplot2::ggplot_build(p)$panel$ranges[[1]]$y.range)
  
  xpos <- mean(diff$diff[diff$cutoff == F])
  if(is.nan(xpos)) xpos <-  mean(diff$diff[diff$cutoff == T])
  
  
  p <- p + ggplot2::annotate('text', x = xpos, y = m / 3, label = paste(prop, '%', sep = "")) +
    ggplot2::xlab('(A - B) / B') +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('Histogram of (A - B) / B Samples : ',
                            name,
                            collapse = "")) +
    ggplot2::theme(legend.position = "none")
  
  print(p) 
  
}

plotSamples <- function(bayesAB, percentLift) {
  for(i in 1:length(bayesAB$posteriors)) {
    obj <- bayesAB$posteriors[[i]]
    samplePlot(obj[[1]], obj[[2]], percentLift, names(obj))
  }
}