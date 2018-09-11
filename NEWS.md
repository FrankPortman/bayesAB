# bayesAB 1.1.1

## Minor Changes
* Bug fixes for most recent version of ggplot2. Hopefully the API is stabe from here on out.
* Fixed `grab` to correctly return the `priors` property in addition to `posteriors` and `inputs`.
* Fixed the `print` generic for the `bayesTestClosed` types to error out informatively

# bayesAB 1.1.0

## Breaking
* Changed conjugate prior of Normal/LogNormal distributions to be the `NormalInverseGamma` distribution from a combination of the `Normal` and `Inverse Gamma` distributions. This distribution is bivariate and gives us a 2d estimate for both `x` and `sig_sq`. The params for this distribution are `mu`, `lambda`, `alpha`, `beta` and are different from the old priors that Normal/LogNormal were expecting.

  * Various doc changes to illustrate these changes and new expectations

## Major Changes
* Fix closed form distributions and added tests
* Calculation Posterior Expected Loss is now correct and represents a true loss function
* Added `plotNormalInvGamma`

## Minor Changes
* Colors for sample plots are now hardcoded (red for > 0 and blue for < 0)
* Plots are truncated at the extremes to avoid very long tails

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
