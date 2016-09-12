# bayesAB

[![Travis-CI Build Status](https://travis-ci.org/FrankPortman/bayesAB.svg?branch=master)](https://travis-ci.org/FrankPortman/bayesAB) [![codecov](https://codecov.io/gh/FrankPortman/bayesAB/branch/master/graph/badge.svg)](https://codecov.io/gh/FrankPortman/bayesAB)


## Fast Bayesian Methods for AB Testing

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

- Decide how you want to parametrize your data (Poisson for counts of email submissions, Bernoulli for CTR on an ad, etc.)
- Use our helper functions to decide on priors for your data (`?bayesTest`)
- Fit a `bayesTest` object
  - Optional: Use `combine` to munge together several `bayesTest` objects together for an arbitrary / non-analytical target distribution
- `print`, `plot`, and `summary` to interpret your results

We also have unit tests so you know this shit is serious.

## Installation

```{r}
install.packages("devtools")
devtools::install_github("frankportman/bayesAB", build_vignettes = TRUE)
```

## Usage

For a more in-depth look please check the vignettes: `browseVignettes(package = "bayesAB")`.

```{r}
library(bayesAB)

# Choose bernoulli test priors
plotBeta(1, 1)
plotBeta(2, 3)

# Choose normal test priors
plotNormal(6, 3)
plotInvGamma(12, 4)

A_binom<- rbinom(100, 1, .5)
B_binom <- rbinom(100, 1, .6)

A_norm <- rnorm(100, 6, 1.5)
B_norm <- rnorm(100, 5, 2.5)

# Fit bernoulli and normal tests
AB1 <- bayesTest(A_binom, B_binom, 
                 priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
AB2 <- bayesTest(A_norm, B_norm, 
                 priors = c('m0' = 5, 'k0' = 1, 's_sq0' = 3, 'v0' = 1), distribution = 'normal')

print(AB1)
summary(AB1)
plot(AB1)

print(AB2)
summary(AB2)
plot(AB2)

# Create a new variable that is the probability multiiplied by the 
# normally distributed variable (expected value of something)
AB3 <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation')

print(AB3)
summary(AB3)
plot(AB3)

```
