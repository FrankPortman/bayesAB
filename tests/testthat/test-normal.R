library(bayesAB)
context('bayesNormalTest')

A_data <- rnorm(100)
B_data <- rnorm(100)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

priors <- c('m0' = 5, 'k0' = 3, 's_sq0' = 3, 'v0' = 2)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors, "jumanji" = 6), distribution = 'normal'),
               "Incorrect length of priors. Expecting an argument for m0, k0, s_sq0, and v0 ONLY.")
  
  expect_error(bayesTest(A_data_bad_string, B_data, priors = priors, distribution = 'normal'),
               "A_data and B_data are not ALL numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'normal'),
               "Arguments don't match requirement for m0, k0, s_sq0, and v0. Check names.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'k0' = -3), distribution = 'normal'),
               "k0 is the 'variance' prior on mu ~ N(m0, k0) and must be strictly positive.", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-3], 's_sq0' = -3), distribution = 'normal'),
               "s_sq0 is the 'alpha' prior on sig_sq ~ InvGamma(s_sq0, v0) and must be strictly positive.", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-4], 'v0' = -3), distribution = 'normal'),
               "v0 is the 'beta' prior on sig_sq ~ InvGamma(s_sq0, v0) and must be strictly positive.", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'normal')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 4")
  expect_output(str(successfulTest), "List of 3")
  
})
