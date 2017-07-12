bayesBernoulliTestClosed <- function(A_data,
                                     B_data,
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

  prob <- bayesBernoulliTestClosed_(sum(A_data) + alpha,
                                    length(A_data) - sum(A_data) + beta,
                                    sum(B_data) + alpha,
                                    length(B_data) - sum(B_data) + beta)

  list(Probability = prob)
}
