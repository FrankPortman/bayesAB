library(bayesAB)
context('bayesPoissonTest')

A_data <- rpois(100, 5)
B_data <- rpois(100, 4)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

priors <- c('shape' = 5, 'rate' = 3)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors, "jumanji" = 6), distribution = 'poisson'),
               "Incorrect number of priors for supplied distribution.")
  
  expect_error(bayesTest(c(A_data, 3.5), B_data, priors = priors, distribution = 'poisson'),
               "as.integer(A_data) == A_data are not all TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'poisson'),
               "Misnamed priors provided for supplied distribution.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = 'cabbage'), distribution = 'poisson'),
               "One or more priors aren't numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = -3), distribution = 'poisson'),
               "shape > 0 is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'rate' = -3), distribution = 'poisson'),
               "rate > 0 is not TRUE", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'poisson')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 5") # inputs
  expect_output(str(successfulTest), "List of 2") # outer
  
})
