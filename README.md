# bayesAB

[![Travis-CI Build Status](https://travis-ci.org/FrankPortman/bayesAB.svg?branch=master)](https://travis-ci.org/FrankPortman/bayesAB) [![codecov](https://codecov.io/gh/FrankPortman/bayesAB/branch/master/graph/badge.svg)](https://codecov.io/gh/FrankPortman/bayesAB)


## Fast Bayesian Methods for A/B Testing in R

bayesAB provides a suite of functions that allow the user to analyze A/B test
data in a Bayesian framework. bayesAB is intended to be a drop-in replacement for
common frequentist hypothesis test such as the t-test and chi-sq test.

Bayesian methods provide several benefits over frequentist methods in the context of
A/B tests - namely in interpretability. Instead of p-values you get direct
probabilities on whether A is better than B (and by how much). Instead of point estimates
your posterior distributions are parametrized random variables which can be summarized
any number of ways. Bayesian tests are also immune to 'peeking' and are thus valid whenever 
a test is stopped.

The general bayesAB workflow is as follows:

- Decide how you wanta to parametrize your data (Poisson for counts of email submissions, Bernoulli for CTR on an ad, etc.)
- Use our helper functions to decide on priors for your data (`?bayesTest`)
- Fit a `bayesTest` object
  - Optional: Use `combine` to munge together several `bayesTest` objects together for an arbitrary / non-analytical target distribution
- `print`, `plot`, and `summary` to interpret your results

We also have unit tests so you know this shit is serious.

## Installation

```{r}
install.packages("devtools")
devtools::install_github("frankportman/bayesAB")
```

## Usage

```{r}
library(bayesAB)

plotBeta(alpha = 1,
         beta = 1)
         
A <- rbinom(250, size = 1, .3)
B <- rbinom(250, size = 1, .2)

AB1 <- bayesTest(A,
                 B,
                     c("alpha" = 1,
                     "beta" = 1),
                     distribution = 'bernoulli')
                     
AB1 <- bayesTest(A, B, c("alpha" = 1, "beta" = 1), distribution = "bernoulli")

liftAB1 <- getMinLift(AB1)

plot(AB1)
 
print(AB1)

print(liftAB1)

A_data <- rnorm(1000,mean = 10,sd = 1)
B_data <- rnorm(1000, mean = 10.1, sd = 3)

AB1Norm <- bayesTest(A_data,
                      B_data,
                      c("m0" = 9,
                      "k0" = 3,
                      "s_sq0" = 1,
                      "v0" = 1),
                      distribution = 'normal')
                      
plot(AB1Norm)

## combine
n <- combine(AB1, AB1Norm, f = `*`, params = c('Probability', 'Mu'), newName = 'Parameter')


#Log Normal Example
A_data <- rlnorm(1000, meanlog = 1.5, sdlog = 1)
B_data <- rlnorm(1000, meanlog = 1.7, sdlog = 1.3)

AB1LogNorm <- bayesLogNormalTest(A_data, B_data, c("m0" = 9,
                      "k0" = 3,
                      "s_sq0" = 1,
                      "v0" = 1))

plot(AB1LogNorm)

## combining

finalRevenue <- AB1 %>%
  combine(AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'E(AdClick)') %>%
  combine(AB3, f = `*`, params = c('E(AdClick)', 'Lambda'), newName = 'E(NumAdClicks)') %>%
  combine(AB4, f = `+`, params = c('E(NumAdClicks)', 'Mu'), newNAme = 'TotalRevenue')

print(finalRevenue)
summary(finalRevenue)
plot(finalRevenue)

```
