library(bayesAB)
context('util')

test_that("Success", {
  
  expect_silent(ppareto(5, 4, 3))
  
  expect_message({detach(package:bayesAB); library(bayesAB)}, "See ?bayesTest for the guts of the bayesAB package and/or ?plotDistributions for help on choosing priors!", fixed = TRUE)
  
})
