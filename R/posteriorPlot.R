posteriorPlotSamp <- function(plotDat, name) {
  
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = value, group = Var2, fill = Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('Test and Control, ',
                            name,
                            'Posteriors',
                            collapse = "")) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(p)
  
}

posteriorPlotClosed <- function(plotDat, name) {
  
  p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = support, y = value, group = Var2, fill=Var2)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = value), alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('Test and Control, ',
                            name,
                            'Posteriors',
                            collapse = "")) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(p)
  
}