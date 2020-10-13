code <- nimbleCode({
  for (t in 1:Nt) {
    
    for (j in 1:Ng) {
      theta[t, j] <- 1/(1+exp(-mu[t, j]))
    }
    
    for (i in 1:Nb) {
      y[i,t] ~ dbin(sum(r[i,1:4] * theta[t,1:4]), n[i,t])
    }
  }
  
  for (j in 1:Ng) {
    mu[1, j] ~ dnorm(0, tau_ini)
  }
  
  for (t in 2:Nt) {
    for (j in 1:Ng) {
      mu[t, j] ~ dnorm(mu[t-1, j], tau_rw)
    }
  }
  
  sig_ini ~ T(dnorm(0, 10), 0, )
  tau_ini <- 1/(sig_ini * sig_ini)
  
  sig_rw ~ T(dnorm(0, 10), 0, )
  tau_rw <- 1/(sig_rw * sig_rw)
})
