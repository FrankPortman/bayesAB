library(bayesAB)
context('bayesLogNormalTest')

A_data <- rlnorm(100)
B_data <- rlnorm(100)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

priors <- c('mu' = 5, 'lambda' = 3, 'alpha' = 3, 'beta' = 2)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors, "jumanji" = 6), distribution = 'lognormal'),
               "Incorrect number of priors for supplied distribution.")
  
  expect_error(bayesTest(A_data_bad_string, B_data, priors = priors, distribution = 'lognormal'),
               "is.numeric(A_data) is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'lognormal'),
               "Misnamed priors provided for supplied distribution.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'lambda' = -3), distribution = 'lognormal'),
               "lambda > 0 is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-3], 'alpha' = -3), distribution = 'lognormal'),
               "alpha > 0 is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-4], 'beta' = -3), distribution = 'lognormal'),
               "beta > 0 is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(c(A_data, -1), B_data, priors = priors, distribution = 'lognormal'),
               "A_data >= 0 are not all TRUE", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'lognormal')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 5") # inputs
  expect_output(str(successfulTest), "List of 2") # outer
  expect_output(str(successfulTest), "List of 4") # inputs
  
})
