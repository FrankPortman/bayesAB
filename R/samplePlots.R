plotBernoulliSamples <- function(test_samples, 
                                 control_samples, 
                                 percent_lift) {
  
  samplePlot(test_samples, control_samples, percent_lift, "Probability")
  
}


plotNormalSamples <- function(test_mus, 
                              control_mus, 
                              test_vars, 
                              control_vars, 
                              percent_lift) {
  
  samplePlot(test_mus, control_mus, percent_lift, "Mu")
  
  samplePlot(test_vars, control_vars, percent_lift, "Variance")
  
}

plotLogNormalSamples <- function(test_mus,
                                 control_mus,
                                 test_vars,
                                 control_vars,
                                 other_statistics,
                                 cutoff = 0) {
  
  
  plotNormalSamples(test_mus, control_mus, test_vars, control_vars, cutoff)
  
  
  
  
}