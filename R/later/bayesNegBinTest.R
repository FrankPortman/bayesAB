bayesNegBinTest <- function(A_data, B_data, m_shape, m_rate, r_shape, r_rate, n_chains, samp_per_chain) {
  
  modelString <- paste0("
model {
for ( i in 1: ", (length(A_data) * 2), " ) {
y[i] ~ dnegbin(p[x[i]] , r[x[i]] )
}
for ( grpIdx in 1:2) {
p[grpIdx] <- r[grpIdx]/(r[grpIdx]+m[grpIdx])
m[grpIdx] ~ dgamma(", m_shape, ", ", m_rate, ")
r[grpIdx] ~ dgamma(", r_shape, ", ", r_rate, ")
v[grpIdx] <- r[grpIdx]*(1-p[grpIdx])/(p[grpIdx]*p[grpIdx])
}
}
")
  
  
  # Load the data:
  y = c(A_data, B_data) # combine data into one vector
  x = c( rep(1,length(A_data)) , rep(2,length(B_data)) ) # create group membership code
  
  # Specify the data in a list, for later shipment to JAGS:
  dataList <- list(
    x = x,
    y = y
  )
  
  
  parameters<-c("m","r","p","v") #collect these
  jagsModel <- jags.model(textConnection(modelString), data=dataList, n.chains=n_chains, n.adapt=1e3)
  codaSamples <- coda.samples(jagsModel, variable.names=parameters, n.iter= samp_per_chain, thin=1)
  m <- as.matrix(codaSamples)
  result <- list(
    posteriors = list(
      A_mean  = m[,1],
      B_mean  = m[,2],
      A_prob  = m[,3],
      B_prob  = m[,4],
      A_r  = m[,5],
      B_r  = m[,6],
      A_var  = m[,7],
      B_var  = m[,8]
    )
  )
  
  
  class(result) <- c('bayesNegBinTest','bayesTest')
  
  return(result)
  
}