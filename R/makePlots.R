# Plot priors based on matching of prior params in bayesAB test object
plotPriors <- function(bayesAB) {
  p <- NULL
  if(bayesAB$inputs$distribution != 'combined') {
    p <- list(do.call(getDistPlotFunc(bayesAB$prior), bayesAB$inputs$priors))
    names(p) <- bayesAB$prior
  }
  p
}

# Plot samples based on lift, name of var, and data
samplePlot <- function(A, B, name, percentLift, f = function(a, b) (a-b)/b) {

  under <- NULL # CRAN NSE hack

  diff <- f(A, B)
  cutoff <- percentLift / 100
  inner <- quantile(diff, c(.005, .995))

  # Always include percentLift in the plot
  inner[1] <- min(inner[1], cutoff)
  inner[2] <- max(inner[2], cutoff)

  diff <- data.frame(diff = diff,
                     under = diff < cutoff,
                     inside = diff >= inner[1] & diff <= inner[2])

  prop <- 1 - sum(diff$under) / nrow(diff)
  prop <- round(prop * 100, digits = 1)

  p <- ggplot2::qplot(diff,
                      data = diff,
                      fill = under,
                      binwidth = diff(range(inner)) / 250,
                      na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c('TRUE' = '#00B6EB', 'FALSE' = '#F8766D')) +
    ggplot2::geom_vline(xintercept = cutoff) +
    ggplot2::xlim(inner[1], inner[2])

  m <- max(ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$y.range)

  xpos <- mean(diff$diff[diff$under == F & diff$inside == T])
  if(is.nan(xpos)) xpos <- mean(diff$diff[diff$under == T & diff$inside == T])

  p <- p + ggplot2::annotate('text', x = xpos, y = m / 3, label = paste(prop, '%', sep = ""), size = 6) +
    ggplot2::xlab('(A - B) / B') +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('Histogram of (A - B) / B Samples : ', name)) +
    ggplot2::guides(fill = FALSE)

  p

}

# Plot posteriors (samples only, not closed form distribution)
posteriorPlot <- function(A, B, name) {

  value <- recipe <- NULL #CRAN NSE hack

  makeDF <- function(dat) data.frame(recipe = deparse(substitute(dat)), value = dat)
  A <- makeDF(A)
  B <- makeDF(B)
  plotDat <- rbind(A, B)

  p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = value, group = recipe, fill = recipe)) +
    ggplot2::geom_density(alpha = 0.75) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('Density') +
    ggplot2::ggtitle(paste0('A and B, ', name, ' Posteriors')) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))

  p

}

# Constructor function for plotSamples and plotPosteriors
plotConstructor <- function(fun, lift) {
  function(bayesAB, ...) {
    out <- vector(mode = 'list', length = length(bayesAB$posteriors))
    names(out) <- names(bayesAB$posteriors)

    lifts <- c(...)

    for(i in seq_along(bayesAB$posteriors)) {
      p <- bayesAB$posteriors[[i]]
      call <- list(A = p$A, B = p$B, name = names(out)[i])
      if(lift) call <- c(call, percentLift = lifts[i])
      pl <- do.call(fun, call)
      pl <- pl + theme_bayesAB()
      pl <- list(pl)
      out[i] <- pl
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

plotSamples <- plotConstructor(samplePlot, TRUE)
plotPosteriors <- plotConstructor(posteriorPlot, FALSE)
