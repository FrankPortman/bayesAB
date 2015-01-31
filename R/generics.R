plot.plda <- function(result, Xnew, y, n.seq = 100, threshold = 5, ...) {    

  
  df <- data.frame(a = result$test_samples, b = result$control_samples)
  df <- melt(df)
  
  
  
}