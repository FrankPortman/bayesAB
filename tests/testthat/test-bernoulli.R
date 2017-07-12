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
               "Data input is incorrect. Data can only contain 0's and 1's. See help docs for more info.", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 1, 'fergalicious' = 1), distribution = 'bernoulli'),
               "Misnamed priors provided for supplied distribution.")
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 'male', 'beta' = 1), distribution = 'bernoulli'),
               "One or more priors aren't numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 0, 'beta' = 1), distribution = 'bernoulli'),
               "alpha and beta are parameters of the Beta Distribution and should be strictly > 0.")

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 5") # inputs
  expect_output(str(successfulTest), "List of 2") # outer
  
})
