code <- nimbleCode({
  for (t in 1:Nt) {
    
    for (j in 1:Ng) {
      theta[t, j] <- 1/(1+exp(-mu[t, j]))
    }
    
    for (i in 1:Nb) {
      y[i,t] ~ dbin(sum(r[i,1:Ng] * theta[t,1:Ng]), n[i,t])
    }
  }
  
  for (j in 1:Ng) {
    mu[1, j] ~ dnorm(0, 0.01)
  }
  
  for (t in 2:Nt) {
    for (j in 1:Ng) {
      mu[t, j] ~ dnorm(mu[t-1, j], tau_rw)
    }
  }
  
  sig_rw ~ T(dnorm(0, 1), 0, )
  tau_rw <- 1/(sig_rw * sig_rw)
})
