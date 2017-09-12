dpareto <- function(x, xm, alpha) ifelse(x > xm , alpha * xm ** alpha / (x ** (alpha + 1)), 0)
ppareto <- function(q, xm, alpha) ifelse(q > xm , 1 - (xm / q) ** alpha, 0 )
qpareto <- function(p, xm, alpha) ifelse(p < 0 | p > 1, NaN, xm * (1 - p) ** (-1 / alpha))
rpareto <- function(n, xm, alpha) qpareto(runif(n), xm, alpha)

qinvgamma_ <- function(area, shape, scale) {
  if((1 - area) <= .Machine$double.eps) {
    return(Inf)
  }
  if(area <= .Machine$double.eps) {
    return(0)
  }
  return(1 / qgamma(1 - area, shape, scale))
}

qinvgamma <- Vectorize(qinvgamma_, vectorize.args = 'area')

dinvgamma_ <- function(x, shape, scale) {
  if (shape <= 0 | scale <= 0) {
    stop("Shape or scale parameter negative in dinvgamma().\n")
  }
  if(x == 0) return(0)
  log.density <- shape * log(scale) - lgamma(shape) - (shape + 1) * log(x) - (scale / x)
  return(exp(log.density))
}

dinvgamma <- Vectorize(dinvgamma_, vectorize.args = 'x')

dNormalInverseGamma <- function(x, sig_sq, mu, lambda, alpha, beta) {
  sqrt(lambda) / (sqrt(sig_sq) *
  sqrt(2 * pi)) * (beta ^ alpha) / gamma(alpha) *
  (1 / sig_sq) ^ (alpha + 1) *
  exp(-1 * (2 * beta + lambda * (x - mu) ^ 2) / (2 * sig_sq) )
}

rNormalInverseGamma <- function(n, mu, lambda, alpha, beta) {
  sig_sq <- 1 / rgamma(n, alpha, beta)
  mu <- rnorm(n, mu, sqrt(sig_sq / lambda))
  list(sig_sq = sig_sq, mu = mu)
}

pNormalInverseGamma <- function(x, sig_sq, mu, lambda, alpha, beta) {
  num <- exp(-1 * (beta) / sig_sq) * (beta / sig_sq) ^ alpha * (erf(sqrt(lambda) * (x - mu) / (sqrt(2) * sqrt(sig_sq)) + 1))
  den <- 2 * sig_sq * gamma(alpha)
  num / den
}

erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
