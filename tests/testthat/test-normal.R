library(bayesAB)
context('bayesNormalTest')

A_data <- rnorm(100)
B_data <- rnorm(100)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

priors <- c('mu' = 5, 'sd' = 3, 'shape' = 3, 'scale' = 2)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors, "jumanji" = 6), distribution = 'normal'),
               "Incorrect number of priors for supplied distribution.")
  
  expect_error(bayesTest(A_data_bad_string, B_data, priors = priors, distribution = 'normal'),
               "is.numeric(A_data) is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'normal'),
               "Misnamed priors provided for supplied distribution.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'sd' = -3), distribution = 'normal'),
               "sd > 0 is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-3], 'shape' = -3), distribution = 'normal'),
               "shape > 0 is not TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-4], 'scale' = -3), distribution = 'normal'),
               "scale > 0 is not TRUE", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'normal')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 5")
  expect_output(str(successfulTest), "List of 2")
  
})
