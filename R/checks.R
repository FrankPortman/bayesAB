checkNumericData <- function(A_data, B_data) stopifnot(is.numeric(A_data), is.numeric(B_data))
checkCompleteData <- function(A_data, B_data) stopifnot(!is.na(A_data), !is.na(B_data))
checkNumericPriors <- function(...) {
  priors <- c(...)
  if(!is.numeric(priors)) stop("One or more priors aren't numeric.")
}

checkPositiveData <- function(A_data, B_data) stopifnot(A_data >= 0, B_data >= 0)
checkNotZeroData <- function(A_data, B_data) stopifnot(A_data != 0, B_data != 0)
checkBinomialData <- function(A_data, B_data) stopifnot(A_data %in% c(0, 1), B_data %in% c(0, 1))
checkIntegerData <- function(A_data, B_data) stopifnot(as.integer(A_data) == A_data, as.integer(B_data) == B_data)

checkGammaPriors <- function(shape, rate, ...) stopifnot(shape > 0, rate > 0)
checkNormalPriors <- function(mu, sd, ...) stopifnot(sd > 0)
checkInvGammaPriors <- function(shape, scale, ...) stopifnot(shape > 0, scale > 0)
checkBetaPriors <- function(alpha, beta, ...) stopifnot(alpha > 0, beta > 0)
checkParetoPriors <- function(xm, alpha, ...) stopifnot(xm > 0, alpha > 0)
