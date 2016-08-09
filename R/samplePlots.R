# plotBernoulliSamples <- function(A_probs, 
#                                  B_probs, 
#                                  percent_lift) {
#   
#   samplePlot(A_probs, B_probs, percent_lift, "P")
#   
# }
# 
# 
# plotNormalSamples <- function(test_mus, 
#                               control_mus, 
#                               test_vars, 
#                               control_vars, 
#                               percent_lift) {
#   
#   samplePlot(test_mus, control_mus, percent_lift, "Mu")
#   
#   samplePlot(test_vars, control_vars, percent_lift, "Sig_Sq")
#   
# }
# 
# plotNormalSamples_ <- function(bayesNorm) {
#   for(i in 1:length(bayesNorm$posteriors)) {
#     obj <- bayesNorm$posteriors[[i]]
#     samplePlot(obj[[1]], obj[[2]], bayesNorm$inputs$percent_lift, names(obj))
#   }
# }
# 
# plotLogNormalSamples_ <- function(bayesLogNorm) {
#   
#   
# }
# 
# plotLogNormalSamples <- function(pos,
#                                  percent_lift = 0) {
#   
#   #Using a for loop rather than lapply since we want to wait for each plot to show:
#   for (i in 1:(length(pos) / 2)) {
#     curr_name <- names(pos)[2*i]
#     metric_name <- paste0(toupper(substr(curr_name, 3, 3)), substr(curr_name, 4, nchar(curr_name)))
#     samplePlot(pos[[2*i-1]], pos[[2*i]], percent_lift, metric_name)
#   }
#   
#   # samplePlot(test_mus, control_mus, percent_lift, "Mu")
#   # samplePlot(test_vars, control_vars, percent_lift, "NormDistVars")
#   # samplePlot(other_statistics$A_means, other_statistics$B_means, percent_lift, "Means")
#   # samplePlot(other_statistics$A_meds, other_statistics$B_meds, percent_lift, "Medians")
#   # samplePlot(other_statistics$A_modes, other_statistics$B_modes, percent_lift, "Modes")
#   # samplePlot(other_statistics$A_vars, other_statistics$B_vars, percent_lift, "Vars")
# 
# }