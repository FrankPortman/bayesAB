constructors <- function() {
  list(
    "bernoulli" = list(
      prior = 'Beta',
      dataChecks = list(
        checkBinomialData
      ),
      priorChecks = list(
        checkBetaPriors
      ),
      posteriors = Bernoulli
    ),

    "bernoulliC" = list(
      prior = 'Beta',
      dataChecks = list(
        checkBinomialData
      ),
      priorChecks = list(
        checkBetaPriors
      ),
      posteriors = BernoulliClosed
    ),

    "normal" = list(
      prior = 'NormalInvGamma',
      dataChecks = list(),
      priorChecks = list(
        checkNormalInvGammaPriors
      ),
      posteriors = Normal
    ),

    "lognormal" = list(
      prior = 'NormalInvGamma',
      dataChecks = list(
        checkPositiveData,
        checkNotZeroData
      ),
      priorChecks = list(
        checkNormalInvGammaPriors
      ),
      posteriors = LogNormal
    ),

    "poisson" = list(
      prior = 'Gamma',
      dataChecks = list(
        checkPositiveData,
        checkIntegerData
      ),
      priorChecks = list(
        checkGammaPriors
      ),
      posteriors = Poisson
    ),

    "poissonC" = list(
      prior = 'Gamma',
      dataChecks = list(
        checkPositiveData,
        checkIntegerData
      ),
      priorChecks = list(
        checkGammaPriors
      ),
      posteriors = PoissonClosed
    ),

    "exponential" = list(
      prior = 'Gamma',
      dataChecks = list(
        checkPositiveData
      ),
      priorChecks = list(
        checkGammaPriors
      ),
      posteriors = Exponential
    ),

    "uniform" = list(
      prior = 'Pareto',
      dataChecks = list(
        checkPositiveData
      ),
      priorChecks = list(
        checkParetoPriors
      ),
      posteriors = Uniform
    )
  )
}

getDistribution <- function(distribution) constructors()[[distribution]]
