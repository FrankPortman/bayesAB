library(bayesAB)
context('bandit')

A_binom <- rbinom(100, 1, .5)
B_binom <- rbinom(100, 1, .6)

AB1 <- bayesTest(A_binom, B_binom, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')

bandit <- banditize(AB1, "Probability")

test_that("Error", {
  
  expect_error(banditize(AB1 + AB1), "Can't turn arbitrary combined distribution into a Bayesian Bandit.",
               fixed = TRUE)
  
})

test_that("Success", {
  
  expect_true(bandit$serveRecipe() %in% c('A', 'B'))
  expect_silent(bandit$getBayesTest())
  expect_silent(bandit$getOriginalTest())
  expect_equal(bandit$setResults(list('A' = 1)), 0)
  expect_output(print(bandit), "updates")
  
})
