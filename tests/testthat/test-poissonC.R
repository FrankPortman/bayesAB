library(bayesAB)
context('bayesPoissonCTest')

A_data <- rpois(100, 5)
B_data <- rpois(100, 4)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

priors <- c('shape' = 5, 'rate' = 3)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors, "jumanji" = 6), distribution = 'poissonC'),
               "Incorrect number of priors for supplied distribution.")
  
  expect_error(bayesTest(c(A_data, 3.5), B_data, priors = priors, distribution = 'poissonC'),
               "as.integer(A_data) == A_data are not all TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'poissonC'),
               "Misnamed priors provided for supplied distribution.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = 'cabbage'), distribution = 'poissonC'),
               "One or more priors aren't numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = -3), distribution = 'poissonC'),
               "shape > 0 is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'rate' = -3), distribution = 'poissonC'),
               "rate > 0 is not TRUE", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'poissonC')
  successfulTest2 <- bayesTest(A_data, B_data, priors = priors, distribution = 'poisson')
  
  
  expect_is(successfulTest, "bayesTestClosed")
  
  expect_output(str(successfulTest), "List of 2") # outer
  expect_output(str(successfulTest), "List of 5") # outer
  p1 <- summary(successfulTest)$probability$Lambda
  p2 <- summary(successfulTest2)$probability$Lambda
  expect_true(abs(p1 - p2) < .01)
  
})
