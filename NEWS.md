#bayesAB v 0.7.0

## Major Additions

- added `banditize` and `deployBandit` to turn your `bayesTest` object into a Bayesian multi-armed bandit and deploy as a JSON API
respectively.
- Added programmatic capabilities on top of existing interactive uses for `plot` generic function
  - You can now assign `plot(bayesTestObj)` to a variable and not have it automatically plot.
- Added quantile summary of calculated posteriors to the output of `summary.bayesTest`
- Added Posterior Expected Loss to output of `summary.bayesTest`
  - This is useful to know when to stop your Bayesian AB Test
  - Supports the risk of choosing 'B' over 'A' (ordering is important) and makes more sense if A > B currently in the test

## Minor Tweaks/Fixes

- outputs from `plot` generics are now explicitly `ggplot` objects and can be modified as such
  - You can input your own titles/axis labels/etc if the defaults don't fit your use case


# bayesAB v 0.6.0

- First major CRAN release
- 6 (+ 2) distributions
- `print`, `plot`, `summary` generics
- Easy plotting of distributions for quick visual inspection
- `combine` tests as needed
- 100% code coverage
