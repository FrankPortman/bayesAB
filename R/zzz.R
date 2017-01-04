.onAttach <- function(...) {
  if (!interactive()) return()
  packageStartupMessage("See ?bayesTest for the guts of the bayesAB package and/or ?plotDistributions for help on choosing priors!")
}
