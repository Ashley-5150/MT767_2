library(statsecol)
library(jagsUI)
library(ggplot2)
data("wildebeest")

set.seed(5150)

#Priors: 
#N1 ~ dunif(0, 2)                 # Initial population size             
#sigma_N ~ dunif(0, 1)            # Initial standard deviation for N 
#sigma_obs ~ dunif(0, 1)          # Initial standard deviation for Observation   
#sigma2_N <- pow(sigma_N, 2)       # Translating it into variation (sd^2)
#sigma2_obs <- pow(sigma_obs, 2)   # Translating it into variation (sd^2)
#tau_N <- pow(sigma_N, -2)         # Translate to precision (sd^-2)
#tau_obs <- pow(sigma_obs, -2)     # Translate to precision (sd^-2)
#beta_0 ~ dnorm(0, 0.01)          # Setting initial beta_0
#beta_1 ~ dnorm(0, 0.01)          # Setting initial beta_1

nyrs <- 30



sink("MT5767_2.txt")
cat("
model{
  # Priors and constraints
  N1 ~ dunif(0, 2)                 # Initial population size  
  N.est[1] <- N1
  sig.N ~ dunif(0, 1)            # Initial standard deviation for N
  tau.N <- pow(sig.N, -2)
  beta.0 ~ dnorm(0, 0.01)          # Setting initial Beta0
  beta.1 ~ dnorm(0, 0.01)          # Setting initial Beta1
  
  for (i in c(2, 4, 6, 8, 12, 13, 18, 19, 23, 25, 27, 30)) {
    sig.y[i] ~ dunif(0, 1)
    tau.y[i] <- pow(sig.y[i], -2)
  }
  
  # Likelihood - State process
  for (t in 1:(nyrs-1)){
    log.r[t] <- beta.0 + beta.1 * rain[t]
    log(r[t]) <- log.r[t]
    N.est[t+1] <- r[t] * N.est[t]
  }
  
  # Likelihood - Observation process
  for(t in c(2, 4, 6, 8, 12, 13, 18, 19, 23, 25, 27, 30)){
    y[t] ~ dnorm(N.est[t], tau.y[t])
  }
}
",fill = TRUE)
sink()

#IS NHAT THE OBSERVATIONS?
wildebeest.data <- list(nyrs = nyrs, y = wildebeest$Nhat, rain = wildebeest$rain)

N.filled <- na.locf(wildebeest$Nhat)

sig.y.filled <- rep(NA, 30)
for (i in c(2, 4, 6, 8, 12, 13, 18, 19, 23, 25, 27, 30)) {
  sig.y.filled[i] <- runif(1, 0, 1)
}

wildebeest.inits <- function(){
  list(
    N1 = runif(1, 0, 2),
    sig.N = runif(1, 0, 1),
    beta.0 = rnorm(1, 0, 0.01),
    beta.1 = rnorm(1, 0, 0.01),
    sig.y = sig.y.filled
  )
}


N = c(Y,rep(Y[years],nproj))



wildebeest.parms <- c("beta.0", "beta.1", "sig.N", "N1", "sig.y")

nc <- 5
nb <- 1000
ni <- 10000 + nb
nt <- 1

wildebeest.out <- jags(data = wildebeest.data,
                       inits = wildebeest.inits,
                       parameters.to.save = wildebeest.parms,
                       model.file = "MT5767_2.txt",
                       n.chains = nc,
                       n.iter = ni,
                       n.burnin = nb,
                       n.thin = nt)

library(MCMCvis)
MCMCtrace(wildebeest.out, params = wildebeest.parms[1:5], type = "trace", iter = ni, pdf = FALSE)

#Runs but need to add initial values of N#
# Betas aren't converging


































