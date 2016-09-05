posteriorPlotSamp <- function(A, B, name) {
  
  ## CRAN hack
  value <- Var2 <- NULL
  
  plotDat <- reshape2::melt(cbind(A,B))
  
  p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = value, group = Var2, fill = Var2)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('A and B, ',
                            name,
                            ' Posteriors',
                            collapse = "")) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  print(p)
  
}

plotPosteriors <- function(bayesAB) {
  for(i in seq_along(bayesAB$posteriors)) {
    p <- bayesAB$posteriors[i]
    n <- names(p)
    p <- unlist(p, recursive = FALSE, use.names = FALSE)
    posteriorPlotSamp(p[[1]], p[[2]], n)
  }
}
