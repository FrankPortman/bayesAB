getProb <- function(A_samples, B_samples, percent_lift = 0) {
  
  mean((100 * (A_samples - B_samples) / B_samples > percent_lift))
  
}