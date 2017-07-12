bayesBernoulliTest <- function(A_data,
                               B_data,
                               n_samples,
                               alpha,
                               beta) {

  ## Check that we only have click data
  if(!(
    all(
      A_data %in% c(0, 1),
      B_data %in% c(0, 1)
      )
    )) {
    stop("Data input is incorrect. Data can only contain 0's and 1's. See help docs for more info.")
  }

  if(!all(c(alpha, beta) > 0)) stop("alpha and beta are parameters of the Beta Distribution and should be strictly > 0.")

  map <- function(data) rbeta(n_samples, sum(data) + alpha, length(data) - sum(data) + beta)

  list(
    Probability = list(A = map(A_data), B = map(B_data))
  )
}
