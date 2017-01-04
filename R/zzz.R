.onAttach <- function(...) {
  if (!interactive()) return() else packageStartupMessage("See ?bayesTest for the guts of the bayesAB package and/or ?plotDistributions for help on choosing priors!")
}
