bayesLogNormalTest <- function(A_data,
                               B_data,
                               n_samples,
                               mu,
                               sd,
                               shape,
                               scale) {

  if((
    any(
      A_data <= 0,
      B_data <= 0
    )
  )) {
    stop("Data input is incorrect. The support of a Log Normal Distribution is (0, Inf).")
  }

  NormalResult <- bayesNormalTest(log(A_data),
                                  log(B_data),
                                  n_samples,
                                  mu,
                                  sd,
                                  shape,
                                  scale)

  ## Means
  A_mus <- NormalResult$Mu$A
  B_mus <- NormalResult$Mu$B

  ## Sigmas
  A_sig_sqs <- NormalResult$Sig_Sq$A
  B_sig_sqs <- NormalResult$Sig_Sq$B

  ## Transform back to log normal for interpretation
  A_means <- exp(A_mus + A_sig_sqs / 2)
  B_means <- exp(B_mus + B_sig_sqs / 2)

  A_vars <- (exp(A_sig_sqs) - 1) * exp(2 * A_mus + A_sig_sqs)
  B_vars <- (exp(B_sig_sqs) - 1) * exp(2 * B_mus + B_sig_sqs)

  list(
    Mu = list(A = A_mus, B = B_mus),
    Sig_Sq = list(A = A_sig_sqs, B = B_sig_sqs),
    Mean = list(A = A_means, B = B_means),
    Var = list(A = A_vars, B = B_vars)
  )
}
