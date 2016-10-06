# Plot priors based on matching of prior params in bayesAB test object
plotPriors <- function(bayesAB, ...) {
  
  funs <- list("beta" = list(params = c('alpha', 'beta'), plotFun = plotBeta),
               "normal" = list(params = c('m0', 'k0'), plotFun = plotNormal),
               "invgamma" = list(params = c('s_sq0', 'v0'), plotFun = plotInvGamma),
               "gamma" = list(params = c('shape', 'rate'), plotFun = plotGamma),
               "pareto" = list(params = c('xm', 'alpha'), plotFun = plotPareto))
  
  vals <- bayesAB$inputs$priors
  labs <- names(vals)
  
  labChecker <- function(...) all(c(...) %in% labs)
  
  out <- list()
  
  for(i in seq_along(funs)) {
    rel <- funs[[i]]
    if(labChecker(rel$params)) {
      pri <- list(do.call(rel$plotFun, as.list(c(unname(vals[rel$params]), ...))))
      names(pri) <- names(funs[i])
      out <- c(out, pri)
    }
  }
  
  out
  
}

# Plot samples based on lift, name of var, and data
samplePlot <- function(A, B, name, percentLift) {
  
  diff <- getLift(A, B)
  cutoff <- percentLift / 100
  
  diff <- data.frame(diff = diff, cutoff = diff < cutoff)
  
  prop <- 1 - sum(diff$cutoff) / nrow(diff)
  prop <- round(prop * 100, digits = 1)
  
  p <- ggplot2::qplot(diff, data = diff, fill = cutoff, binwidth = diff(range(diff)) / 250) + 
    ggplot2::geom_vline(xintercept = cutoff)
  
  ## ugly ggplot2 update fix
  ## deprecate in > 2.2 CRAN release
  m <- ifelse(packageVersion("ggplot2") >= "2.1.0.9001",
              max(ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$y.range),
              max(ggplot2::ggplot_build(p)$panel$ranges[[1]]$y.range))

  xpos <- mean(diff$diff[diff$cutoff == F])
  if(is.nan(xpos)) xpos <-  mean(diff$diff[diff$cutoff == T])
  
  
  p <- p + ggplot2::annotate('text', x = xpos, y = m / 3, label = paste(prop, '%', sep = "")) +
    ggplot2::xlab('(A - B) / B') +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('Histogram of (A - B) / B Samples : ',
                            name,
                            collapse = "")) +
    ggplot2::theme(legend.position = "none")
  
  p 
  
}

# Plot posteriors (samples only, not closed form distribution)
posteriorPlot <- function(A, B, name) {
  
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
  
  p
  
}

# Constructor function for plotSamples and plotPosteriors
plotConstructor <- function(fun, ...) {
  function(bayesAB, ...) {
    out <- list()
    for(i in seq_along(bayesAB$posteriors)) {
      p <- bayesAB$posteriors[i]
      n <- names(p)
      p <- unlist(p, recursive = FALSE, use.names = FALSE)
      pl <- fun(A = p[[1]], B = p[[2]], name = n, ...) + theme_bayesAB()
      pl <- list(pl)
      names(pl) <- n
      out <- c(out, pl)
    }
    return(out)
  }
}

plotSamples <- plotConstructor(samplePlot, percentLift)

plotPosteriors <- plotConstructor(posteriorPlot)
