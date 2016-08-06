plotBernoulliSamples <- function(test_samples, 
                                 control_samples, 
                                 percent_lift) {
  
  samplePlot(test_samples, control_samples, percent_lift, "P")
  
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
                                 percent_lift = 0) {
  
  samplePlot(test_mus, control_mus, percent_lift, "Mu")
  samplePlot(test_vars, control_vars, percent_lift, "NormDistVars")
  samplePlot(other_statistics$A_means, other_statistics$B_means, percent_lift, "Means")
  samplePlot(other_statistics$A_meds, other_statistics$B_meds, percent_lift, "Medians")
  samplePlot(other_statistics$A_modes, other_statistics$B_modes, percent_lift, "Modes")
  samplePlot(other_statistics$A_vars, other_statistics$B_vars, percent_lift, "Vars")

}