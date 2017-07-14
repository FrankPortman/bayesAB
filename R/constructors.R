constructors <- function() {
  list(
    "bernoulli" = list(
      dataChecks = list(
        checkBinomialData
      ),
      priorChecks = list(
        checkBetaPriors
      ),
      posteriors = Bernoulli
    ),

    "bernoulliC" = list(
      dataChecks = list(
        checkBinomialData
      ),
      priorChecks = list(
        checkBetaPriors
      ),
      posteriors = BernoulliClosed
    ),

    "normal" = list(
      dataChecks = list(),
      priorChecks = list(
        checkNormalPriors,
        checkInvGammaPriors
      ),
      posteriors = Normal
    ),

    "lognormal" = list(
      dataChecks = list(
        checkPositiveData,
        checkNotZeroData
      ),
      priorChecks = list(
        checkNormalPriors,
        checkInvGammaPriors
      ),
      posteriors = LogNormal
    ),

    "poisson" = list(
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
      dataChecks = list(
        checkPositiveData
      ),
      priorChecks = list(
        checkGammaPriors
      ),
      posteriors = Exponential
    ),

    "uniform" = list(
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
