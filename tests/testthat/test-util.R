library(bayesAB)
context('util')

test_that("Success", {

  expect_silent(ppareto(5, 4, 3))
  expect_equal(removeGenericArgs(c('A_data')), character(0))
  expect_equal(removeGenericArgs(c('A_data', 'hi')), 'hi')

})
