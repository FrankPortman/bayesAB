# bayesAB 1.0.0

## Major Additions

* Added `grab` and `rename` to retrieve and rename posteriors from your `bayesTest` object

  * Mostly useful in conjunction with `combine` in order to quickly chain together several `bayesTest`s
  
* Correctly hide legend for generic plots
* Standardized prior parameters to have the same arguments as the `plot{Dist}` functions

  * This mostly changes prior inputs for `bayesTest(distribution = c('normal', 'lognormal'))`
  * This is a **breaking** change

## Major Internal Changes

* Moved `distribution` metadata from `bayesTest$distribution` to `bayesTest$inputs$distribution` to be consistent
* Reconcile posterior names to always be `A` and `B` and not include the parameter name
* `A_data` and `B_data` in inputs are now always lists by default to make `combine` work more simply
* Big refactor of how `bayesTest` works internally. Dispatch per distribution is now only related to how the posterior is calculated.
* Some error checking has been made more generic

## Minor Tweaks/Fixed

* Posterior Expected Loss now correctly displays 0 instead of NaN for that case
* Numerous doc/examples/tests cleanup
* Overall refactor of some methods, making it easier to read and contribute

# bayesAB 0.7.0

## Major Additions

* added `banditize` and `deployBandit` to turn your `bayesTest` object into a Bayesian multi*armed bandit and deploy as a JSON API respectively.
* Added programmatic capabilities on top of existing interactive uses for `plot` generic function

  * You can now assign `plot(bayesTestObj)` to a variable and not have it automatically plot.
  
* Added quantile summary of calculated posteriors to the output of `summary.bayesTest`
* Added Posterior Expected Loss to output of `summary.bayesTest`

  * This is useful to know when to stop your Bayesian AB Test
  * Supports the risk of choosing 'B' over 'A' (ordering is important) and makes more sense if A > B currently in the test

## Minor Tweaks/Fixes

* outputs from `plot` generics are now explicitly `ggplot` objects and can be modified as such

  * You can input your own titles/axis labels/etc if the defaults don't fit your use case

# bayesAB 0.6.0

## Major Additions

* First major CRAN release
* 6 (+ 2) distributions
* `print`, `plot`, `summary` generics
* Easy plotting of distributions for quick visual inspection
* `combine` tests as needed
* 100% code coverage
