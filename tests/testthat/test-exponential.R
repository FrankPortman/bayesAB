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
               "Incorrect length of priors. Expecting an argument for shape and rate ONLY.")
  
  expect_error(bayesTest(c(A_data, -3), B_data, priors = priors, distribution = 'exponential'),
               "Data input is incorrect. The support of an Exponential Distribution is [0, Inf).", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'exponential'),
               "Arguments don't match requirement for shape and rate. Check names.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = 'cabbage'), distribution = 'exponential'),
               "shape and/or rate are not numeric!")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'shape' = -3), distribution = 'exponential'),
               "shape and rate are parameters of the Gamma Distribution and should be strictly > 0.", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'rate' = -3), distribution = 'exponential'),
               "shape and rate are parameters of the Gamma Distribution and should be strictly > 0.", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'exponential')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 4")
  expect_output(str(successfulTest), "List of 3")
  
})
