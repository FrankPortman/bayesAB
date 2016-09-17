library(bayesAB)
context('generics')

A_data <- rlnorm(100)
B_data <- rlnorm(100)

priors <- c('m0' = 5, 'k0' = 3, 's_sq0' = 3, 'v0' = 2)

x <- bayesTest(A_data, B_data, priors = priors, distribution = 'lognormal')

A_binom<- rbinom(100, 1, .5)
B_binom <- rbinom(100, 1, .6)

AB3 <- bayesTest(A_binom, B_binom, 
                 priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulliC')

test_that("Failures based on inputs", {
  
  expect_error(plot(x, percentLift = 0), "Must supply a 'percentLift' for every parameter with a posterior distribution.")
  
  expect_error(plot(x, priors = FALSE, posteriors = FALSE, samples = FALSE), "Must specifiy at least one plot to make.")
  
  expect_error(summary(x, percentLift = 0), "Must supply a 'percentLift' for every parameter with a posterior distribution.")
  
  expect_error(summary(x, credInt = 0), "Must supply a 'credInt' for every parameter with a posterior distribution.")

})

test_that("Success", {
  
  expect_silent(plot(x))
  expect_silent(plot(x, rep(.5, 4)))
  
  expect_silent(print(plot(x)))
  expect_silent(print(plot(x, rep(.5, 4))))
  
  expect_output(print(x), "Distribution used")
  
  expect_silent(summary(x))
  expect_silent(summary(x, rep(.5, 4)))
  expect_silent(summary(x, rep(.5, 4), rep(.5, 4)))
  
  expect_output(print(summary(x)), 'P(A > B)', fixed = TRUE)
  expect_output(print(summary(AB3)), 'P(A > B)', fixed = TRUE)
  
})
