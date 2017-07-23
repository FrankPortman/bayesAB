# Plot priors based on matching of prior params in bayesAB test object
plotPriors <- function(bayesAB) {

  funs <- list(
    "beta"     = plotBeta,
    "normal"   = plotNormal,
    "invgamma" = plotInvGamma,
    "gamma"    = plotGamma,
    "pareto"   = plotPareto
  )

  vals <- bayesAB$inputs$priors
  labs <- names(vals)

  labChecker <- function(...) all(c(...) %in% labs)

  out <- list()

  for(name in names(funs)) {
    fun <- funs[[name]]
    inputs <- names(formals(fun))
    if(labChecker(inputs)) {
      pri <- do.call(fun, as.list(vals[inputs]))
      pri <- list(pri)
      names(pri) <- name
      out <- c(out, pri)
    }
  }

  out

}

# Plot samples based on lift, name of var, and data
samplePlot <- function(A, B, name, percentLift) {

  diff <- getLift(A, B)
  cutoff <- percentLift / 100
  inner <- quantile(diff, c(.01, .99))

  diff <- data.frame(diff = diff,
                     cutoff = diff < cutoff,
                     inside = diff >= inner[1] & diff <= inner[2])

  prop <- 1 - sum(diff$cutoff) / nrow(diff)
  prop <- round(prop * 100, digits = 1)

  p <- ggplot2::qplot(diff,
                     data = diff,
                     fill = cutoff,
                     binwidth = diff(range(inner)) / 250,
                     na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = cutoff) +
    ggplot2::xlim(inner[1], inner[2])

  m <- max(ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$y.range)

  xpos <- mean(diff$diff[diff$cutoff == F & diff$inside == T])
  if(is.nan(xpos)) xpos <- mean(diff$diff[diff$cutoff == T & diff$inside == T])

  p <- p + ggplot2::annotate('text', x = xpos, y = m / 3, label = paste(prop, '%', sep = ""), size = 6) +
    ggplot2::xlab('(A - B) / B') +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('Histogram of (A - B) / B Samples : ', name)) +
    ggplot2::guides(fill = FALSE)

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
    ggplot2::ggtitle(paste0('A and B, ', name, ' Posteriors')) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))

  p

}

# Constructor function for plotSamples and plotPosteriors
plotConstructor <- function(fun, ...) {
  function(bayesAB, ...) {
    out <- list()
    for(name in names(bayesAB$posteriors)) {
      p <- bayesAB$posteriors[[name]]
      pl <- fun(A = p$A, B = p$B, name = name, ...) + theme_bayesAB()
      pl <- list(pl)
      names(pl) <- name
      out <- c(out, pl)
    }
    return(out)
  }
}

theme_bayesAB <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                   title = ggplot2::element_text(size = 12))
}

plotSamples <- plotConstructor(samplePlot, percentLift)
plotPosteriors <- plotConstructor(posteriorPlot)
