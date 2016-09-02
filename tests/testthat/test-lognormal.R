library(bayesAB)
context('bayesLogNormalTest')

A_data <- rlnorm(100)
B_data <- rlnorm(100)

A_data_bad_string <- c(A_data, "porcupine")
A_data_bad_prop <- c(A_data, .3)
A_data_bad_non_unique <- c(A_data, 2)

priors <- c('m0' = 5, 'k0' = 3, 's_sq0' = 3, 'v0' = 2)

test_that("Failures based on input types", {
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors, "jumanji" = 6), distribution = 'lognormal'),
               "Incorrect length of priors. Expecting an argument for m0, k0, s_sq0, and v0 ONLY.")
  
  expect_error(bayesTest(A_data_bad_string, B_data, priors = priors, distribution = 'lognormal'),
               "A_data and B_data are not ALL numeric.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-1], 'fergalicious' = 1), distribution = 'lognormal'),
               "Arguments don't match requirement for m0, k0, s_sq0, and v0. Check names.")
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-2], 'k0' = -3), distribution = 'lognormal'),
               "k0 is the 'variance' prior on mu ~ N(m0, k0) and must be strictly positive.", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-3], 's_sq0' = -3), distribution = 'lognormal'),
               "s_sq0 is the 'alpha' prior on sig_sq ~ InvGamma(s_sq0, v0) and must be strictly positive.", fixed = TRUE)
  
  expect_error(bayesTest(A_data, B_data, priors = c(priors[-4], 'v0' = -3), distribution = 'lognormal'),
               "v0 is the 'beta' prior on sig_sq ~ InvGamma(s_sq0, v0) and must be strictly positive.", fixed = TRUE)
  
  expect_error(bayesTest(c(A_data, -1), B_data, priors = priors, distribution = 'lognormal'),
               "Data input is incorrect. The support of a Log Normal Distribution is (0, Inf).", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- bayesTest(A_data, B_data, priors = priors, distribution = 'lognormal')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 4") # inputs
  expect_output(str(successfulTest), "List of 3") # outer
  
})
