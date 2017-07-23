library(bayesAB)
context('bayesExponentialTest')

A_data <- rexp(100)
B_data <- rexp(100)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

priors <- c('shape' = 3, 'rate' = 5)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors, "jumanji" = 6), distribution = 'exponential'),
               "Incorrect number of priors for supplied distribution.")
  
  expect_error(bayesTest(c(A_data, -3), B_data, priors = priors, distribution = 'exponential'),
               "A_data >= 0 are not all TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'exponential'),
               "Misnamed priors provided for supplied distribution.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = 'cabbage'), distribution = 'exponential'),
               "One or more priors aren't numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = -3), distribution = 'exponential'),
               "shape > 0 is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'rate' = -3), distribution = 'exponential'),
               "rate > 0 is not TRUE", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'exponential')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 2")
  expect_output(str(successfulTest), "List of 5")
  
})
