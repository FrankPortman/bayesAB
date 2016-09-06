# bayesAB

[![Travis-CI Build Status](https://travis-ci.org/FrankPortman/bayesAB.svg?branch=master)](https://travis-ci.org/FrankPortman/bayesAB) [![codecov](https://codecov.io/gh/FrankPortman/bayesAB/branch/master/graph/badge.svg)](https://codecov.io/gh/FrankPortman/bayesAB)


##AB Testing for Counts and Proportions data using Bayesian Methods

bayesAB provides a suite of functions that allow the user to analyze
AB test data in a similar light to common frequentist hypothesis tests
(t-test, proportion test, etc.).

bayesAB contains functions for choosing an informative prior and will
allow you to directly state the probability P(A > B) for counts and
proportions. This has several implications, namely in terms of
interpretability. Frequentist t-tests that rely on p-values are
notoriously hard to interpret for statisticians and non-statisticians
alike. Bayesian methods are also immune to 'peeking' and are thus
valid no matter when a test is stopped.

We also have unit tests so you know this shit is serious.

### To Do

#### V1

- Documentation
- Vignette for usage
- Hook up 'closed forms'
- Programmatic usage for plots - specifying which you want or being able to extract those objects safely
- Tests

#### V2

- More distributions

## Installation

```{r}
install.packages("devtools")
devtools::install_github("frankportman/bayesAB")
```

## Useage

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

```
