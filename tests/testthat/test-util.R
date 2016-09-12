library(bayesAB)
context('util')

test_that("Success", {
  
  expect_silent(ppareto(5, 4, 3))
  
})
