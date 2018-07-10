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
                 priors = c('mu' = 5, 'lambda' = 1, 'alpha' = 3, 'beta' = 1), distribution = 'normal')

AB3 <- bayesTest(A_binom, B_binom, 
                 priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulliC')

AB4 <- bayesTest(A_norm, B_norm, 
                 priors = c('mu' = 5, 'lambda' = 1, 'alpha' = 3, 'beta' = 1), distribution = 'normal', n_samples = 1e3)

test_that("Failures based on input types", {
  
  expect_error(combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu', 'lol'), newName = 'Expectation'),
               'You must specify only (2) params - one for the first test and one for the second test.', fixed = TRUE)
  
  expect_error(combine(AB1, AB2, f = `*`, params = c('ProbabilityZ', 'Mu'), newName = 'Expectation'),
               "You have specified a `param` name that doesn't exist in the posterior of the first test and/or the second test.",
               fixed = TRUE)
  
  expect_error(combine(AB1, AB3, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation'),
               "Can't combine a 'closed' bayesTest.", fixed = TRUE)
  
  expect_warning(combine(AB1, AB4, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation'),
                 "n_samples not equal. Make sure `f` handles recycling appropriately.", fixed = TRUE)

  expect_error(grab(AB2, 'MU'), "That posterior doesn't exist in the input bayesTest.", fixed = TRUE)

  expect_error(rename(AB2, 'lol'), 'Can only rename bayesTests with one posterior.', fixed = TRUE)

})

test_that("Success", {
  
  successfulTestMul <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'))
  successfulTestAdd <- combine(AB1, AB2, f = `+`, params = c('Probability', 'Mu'))
  successfulTestSub <- combine(AB1, AB2, f = `-`, params = c('Probability', 'Mu'))
  successfulTestDiv <- combine(AB1, AB2, f = `/`, params = c('Probability', 'Mu'))
  
  successfulTestMulRename <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation')
  successfulTestMulRename2 <- rename(AB1 * grab(AB2, 'Mu'), 'Expectation')
  
  expect_is(successfulTestMul, "bayesTest")
  
  expect_output(str(successfulTestMul), "List of 5") # inputs
  expect_output(str(successfulTestMul), "List of 2") # outer
  
  expect_identical(successfulTestMul, AB1 * grab(AB2, 'Mu'))
  expect_identical(successfulTestAdd, AB1 + grab(AB2, 'Mu'))
  expect_identical(successfulTestSub, AB1 - grab(AB2, 'Mu'))
  expect_identical(successfulTestDiv, AB1 / grab(AB2, 'Mu'))
  
  expect_identical(successfulTestMulRename, successfulTestMulRename2)

  expect_equal(3, length(grab(AB2, 'Mu')))
  
})
