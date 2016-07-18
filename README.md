# bayesAB

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

AB1 <- bayesBernoulliTest(clicks_test = 75,
                     views_test = 250,
                     clicks_control = 50,
                     views_control = 250,
                     alpha = 1,
                     beta = 1)

liftAB1 <- getMinLift(AB1)

plot(AB1)
 
print(AB1)

print(liftAB1)

A_data <- rnorm(1000,mean = 10,sd = 1)
B_data <- rnorm(1000, mean = 10.1, sd = 3)

AB1Norm <- bayesNormalTest(A_data,
                      B_data,
                      9,
                      3,
                      1,
                      1)
                      
plot(AB1Norm)

#Log Normal Example
A_data <- rlnorm(1000, meanlog = 1.5, sdlog = 1)
B_data <- rlnorm(1000, meanlog = 1.7, sdlog = 1.3)

AB2Norm <- bayesLogNormalTest(A_data, B_data, 1.6, 3, 1, 1)

plot(AB2Norm)

#Negative Binomial Example
A_data <- rnbinom(1000, 3, 0.8)
B_data <- rnbinom(1000, 3.5, 0.9)

AB_NB <- bayesNegBinTest(A_data, B_data, .01, .01, .01, .01, 3, 1000)

plot(AB_NB)

```