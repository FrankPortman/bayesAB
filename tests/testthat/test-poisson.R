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
               "Incorrect length of priors. Expecting an argument for shape and rate ONLY.")
  
  expect_error(bayesTest(c(A_data, 3.5), B_data, priors = priors, distribution = 'poisson'),
               "Data input is incorrect. The support of a Poisson distribution is Z*.", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'poisson'),
               "Arguments don't match requirement for shape and rate. Check names.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = 'cabbage'), distribution = 'poisson'),
               "shape and/or rate are not numeric!")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = -3), distribution = 'poisson'),
               "shape and rate are parameters of the Gamma Distribution and should be strictly > 0.", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'rate' = -3), distribution = 'poisson'),
               "shape and rate are parameters of the Gamma Distribution and should be strictly > 0.", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'poisson')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 4") # inputs
  expect_output(str(successfulTest), "List of 3") # outer
  
})
