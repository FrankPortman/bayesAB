# bayesAB

[![Travis-CI Build Status](https://travis-ci.org/FrankPortman/bayesAB.svg?branch=master)](https://travis-ci.org/FrankPortman/bayesAB) [![codecov](https://codecov.io/gh/FrankPortman/bayesAB/branch/master/graph/badge.svg)](https://codecov.io/gh/FrankPortman/bayesAB) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bayesAB)](https://CRAN.R-project.org/package=bayesAB)


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
- Use our helper functions to decide on priors for your data (`?bayesTest`, `?plotDistributions`)
- Fit a `bayesTest` object
  - Optional: Use `combine` to munge together several `bayesTest` objects together for an arbitrary / non-analytical target distribution
- `print`, `plot`, and `summary` to interpret your results
  - Determine whether to stop your test early given the Posterior Expected Loss in `summary` output

Optionally, use `banditize` and/or `deployBandit` to turn a pre-calculated (or empty) `bayesTest` into a multi-armed bandit that can serve recipe recommendations and adapt as new data comes in.

Note, while bayesAB was designed to exploit data related to A/B/etc tests, you can use the package to conduct Bayesian analysis on virtually any vector of data, as long as it can be parametrized by the available functions.

## Installation

Get the latest stable release from CRAN:
```{r}
install.packages("bayesAB")
```

Or the dev version straight from Github:
```{r}
install.packages("devtools")
devtools::install_github("frankportman/bayesAB", build_vignettes = TRUE)
```

## Usage

Some useful links from my blog with `bayesAB` examples (and pictures!!):

- [bayesAB: A New R Package for Bayesian AB Testing](http://fportman.com/blog/bayesab-a-new-r-package-for-bayesian-ab-testing/)
- [bayesAB 0.7.0 + a Primer on Priors](http://fportman.com/blog/bayesab-0-dot-7-0-plus-a-primer-on-priors/)

For a more in-depth look please check the package vignettes with `browseVignettes(package = "bayesAB")` or the pre-knit HTML version on CRAN [here](https://CRAN.R-project.org/package=bayesAB/vignettes/introduction.html). Brief example below. Run the following code for a quick overview of bayesAB:

```{r}
library(bayesAB)

# Choose bernoulli test priors
plotBeta(2, 3)

```

![image](https://cloud.githubusercontent.com/assets/2442871/21627768/f7081684-d1ce-11e6-825f-b4a7b43cd40b.png)

```{r}
# Choose normal test priors
plotInvGamma(12, 4)

```

![image](https://cloud.githubusercontent.com/assets/2442871/21627773/02271ccc-d1cf-11e6-9d35-4b4896112cb3.png)

```{r}
A_binom <- rbinom(100, 1, .5)
B_binom <- rbinom(100, 1, .55)

# Fit bernoulli and normal tests
AB1 <- bayesTest(A_binom, 
                 B_binom, 
                 priors = c('alpha' = 1, 'beta' = 1), 
                 distribution = 'bernoulli')
                 
plot(AB1)

```

![image](https://cloud.githubusercontent.com/assets/2442871/21627830/8f3c4ed4-d1cf-11e6-853f-d02d79077c91.png) ![image](https://cloud.githubusercontent.com/assets/2442871/21627834/94cf94d2-d1cf-11e6-844b-7f3c1684e2f0.png) ![image](https://cloud.githubusercontent.com/assets/2442871/21627838/9a8a1a0a-d1cf-11e6-8542-d901f47adc89.png)

```{r}
print(AB1)

```

```
--------------------------------------------
Distribution used: bernoulli 
--------------------------------------------
Using data with the following properties: 
        [,1] [,2]
Min.    0.00 0.00
1st Qu. 0.00 0.00
Median  1.00 1.00
Mean    0.55 0.56
3rd Qu. 1.00 1.00
Max.    1.00 1.00
--------------------------------------------
Priors used for the calculation: 
alpha  beta 
    1     1 
--------------------------------------------
Calculated posteriors for the following parameters: 
Probability 
--------------------------------------------
Monte Carlo samples generated per posterior: 
[1] 1e+05
```

```{r}
summary(AB1)

```

```
Quantiles of posteriors for A and B:

$Probability
$Probability$A_probs
       0%       25%       50%       75%      100% 
0.3201523 0.5157494 0.5493068 0.5827143 0.7538153 

$Probability$B_probs
       0%       25%       50%       75%      100% 
0.3539916 0.5262891 0.5595286 0.5928125 0.7506643 


--------------------------------------------

P(A > B) by (0)%: 

$Probability
[1] 0.44251

--------------------------------------------

Credible Interval on (A - B) / B for interval length(s) (0.9) : 

$Probability
        5%        95% 
-0.2036608  0.2108876 

--------------------------------------------

Posterior Expected Loss for choosing B over A:

$Probability
[1] 0.3254871

```
