library(bayesAB)
context('bayesUniformTest')

A_data <- runif(100, max = 20)
B_data <- runif(100, max = 15)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

priors <- c('xm' = 17, 'alpha' = 5)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors, "jumanji" = 6), distribution = 'uniform'),
               "Incorrect length of priors. Expecting an argument for xm and alpha ONLY.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'uniform'),
               "Arguments don't match requirement for xm and alpha. Check names.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'alpha' = -3), distribution = 'uniform'),
               "xm and alpha are parameters of the Pareto Distribution and should be strictly > 0.", fixed = TRUE)
  
  expect_error(bayesTest(c(A_data, -1), B_data, priors = priors, distribution = 'uniform'),
               "Data input is incorrect. The support of a Uniform distribution in bayesAB is (0, Inf).", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'uniform')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 4") # inputs
  expect_output(str(successfulTest), "List of 3") # outer
  
})
