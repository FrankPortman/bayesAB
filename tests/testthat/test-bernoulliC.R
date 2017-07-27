library(bayesAB)
context('bayesBernoulliCTest')

A_data <- rbinom(100, 1, .5)
B_data <- rbinom(100, 1, .6)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 5, 'beta' = 3, "jumanji" = 6), distribution = 'bernoulliC'),
               "Incorrect number of priors for supplied distribution.")
  
  expect_error(bayesTest(c(A_data, -3), B_data, priors = c('alpha' = 5, 'beta' = 3), distribution = 'bernoulliC'),
               "A_data %in% c(0, 1) are not all TRUE", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 1, 'fergalicious' = 1), distribution = 'bernoulliC'),
               "Misnamed priors provided for supplied distribution.")
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 'male', 'beta' = 1), distribution = 'bernoulliC'),
               "One or more priors aren't numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 0, 'beta' = 1), distribution = 'bernoulliC'),
               "alpha > 0 is not TRUE")

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulliC')
  successfulTest2 <- bayesTest(A_data, B_data, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
  
  expect_is(successfulTest, "bayesTestClosed")
  
  expect_output(str(successfulTest), "List of 2") # outer
  expect_output(str(successfulTest), "List of 5") # inputs
  p1 <- summary(successfulTest)$probability$Probability
  p2 <- summary(successfulTest2)$probability$Probability
  expect_true(abs(p1 - p2) < .01)
  
})
