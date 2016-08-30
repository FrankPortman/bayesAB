library(bayesAB)
context('bayesBernoulliTest')

A_data <- rbinom(100, 1, .5)
B_data <- rbinom(100, 1, .6)

test_that("Wrong number of priors fails", {
  expect_error(bayesTest(A_data, B_data, priors = c('alpha' = 5, 'beta' = 3, "jumanji" = 6), distribution = 'bernoulli'),
               "Incorrect length of priors. Expecting an argument for alpha and beta")
})