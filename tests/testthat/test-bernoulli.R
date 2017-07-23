library(bayesAB)
context('bayesBernoulliTest')

A_data <- rbinom(100, 1, .5)
B_data <- rbinom(100, 1, .6)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 5, 'beta' = 3, "jumanji" = 6), distribution = 'bernoulli'),
               "Incorrect number of priors for supplied distribution.")
  
  expect_error(bayesTest(c(A_data, -3), B_data, priors = c('alpha' = 5, 'beta' = 3), distribution = 'bernoulli'),
               "A_data %in% c(0, 1) are not all TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 1, 'fergalicious' = 1), distribution = 'bernoulli'),
               "Misnamed priors provided for supplied distribution.")
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 'male', 'beta' = 1), distribution = 'bernoulli'),
               "One or more priors aren't numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = list('alpha' = 'male', 'beta' = 1), distribution = 'bernoulli'),
               "One or more priors aren't numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 1, 'beta' = 0), distribution = 'bernoulli'),
               "beta > 0 is not TRUE")

})

test_that("Success", {
  
  successfulTest <-     bayesTest(A_data, B_data, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
  successfulTestList <- bayesTest(A_data, B_data, priors = list('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
  
  expect_is(successfulTest, "bayesTest")
  expect_is(successfulTestList, "bayesTest")
  
  expect_output(str(successfulTest), "List of 5") # inputs
  expect_output(str(successfulTest), "List of 2") # outer
  expect_output(str(successfulTestList), "List of 5") # inputs
  expect_output(str(successfulTestList), "List of 2") # outer
})
