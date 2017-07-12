bayesUniformTest <- function(A_data,
                             B_data,
                             n_samples,
                             xm,
                             alpha) {

  ###
  ## Error Checking
  ###

  ## Check that we only have positive data
  if((
    any(
      A_data <= 0,
      B_data <= 0
    )
  )) {
    stop("Data input is incorrect. The support of a Uniform distribution in bayesAB is (0, Inf).")
  }

  if(!all(c(xm, alpha) > 0)) stop("xm and alpha are parameters of the Pareto Distribution and should be strictly > 0.")

  ###
  ## Sample from posterior
  ###

  map <- function(data) rpareto(n_samples, max(data, xm), length(data) + alpha)

  ###
  ## Output the result
  ###

  result <- list(
    inputs = as.list(match.call()[-1]),

    posteriors = list(
      Theta = list(A = map(A_data), B = map(B_data))
    ),

    distribution = "uniform"
  )

  class(result) <- c('bayesTest')

  return(result)

}
