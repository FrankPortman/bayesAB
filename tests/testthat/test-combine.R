library(bayesAB)
context('combine')

A_binom<- rbinom(100, 1, .5)
B_binom <- rbinom(100, 1, .6)

A_norm <- rnorm(100, 6, 1.5)
B_norm <- rnorm(100, 5, 2.5)

# Fit bernoulli and normal tests
AB1 <- bayesTest(A_binom, B_binom, 
                 priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')

AB2 <- bayesTest(A_norm, B_norm, 
                 priors = c('m0' = 5, 'k0' = 1, 's_sq0' = 3, 'v0' = 1), distribution = 'normal')

AB3 <- bayesTest(A_binom, B_binom, 
                 priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulliC')

AB4 <- bayesTest(A_norm, B_norm, 
                 priors = c('m0' = 5, 'k0' = 1, 's_sq0' = 3, 'v0' = 1), distribution = 'normal', n_samples = 1e3)

test_that("Failures based on input types", {
  
  expect_error(combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu', 'lol'), newName = 'Expectation'),
               'You must specify only (2) params. One for the first test and one for the second test.', fixed = TRUE)
  
  expect_error(combine(AB1, AB2, f = `*`, params = c('ProbabilityZ', 'Mu'), newName = 'Expectation'),
               "You have specified a `param` name that doesn't exist in the posterior of the first test and/or the second test.",
               fixed = TRUE)
  
  expect_error(combine(AB1, AB3, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation'),
               "Can't combine a 'closed' bayesTest.", fixed = TRUE)
  
  expect_warning(combine(AB1, AB4, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation'),
                 "n_samples not equal. Make sure `f` handles recycling appropriately.", fixed = TRUE)

})

test_that("Success", {
  
  successfulTest <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation')
  
  expect_is(successfulTest, "bayesTest")
  
  expect_output(str(successfulTest), "List of 4") # inputs
  expect_output(str(successfulTest), "List of 3") # outer
  
})
