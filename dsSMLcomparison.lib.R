require("MASS")

fxi <- function(psi, eta, Y){
  
  truePos <- rbinom(n = length(Y), size = 1, prob = psi)
  trueNeg <- rbinom(n = length(Y), size = 1, prob = 1 - eta)
  
  f <- Y
  f[f==1] <- truePos[f==1]
  f[f==0] <- trueNeg[f==0]
  
  return(f)
}

simResponseData <- function(N = 1000, M = 10, b = .3, accMin = .5, accMax = .8){
  
  Psi <- runif( M, min = accMin, max = accMax)
  Eta <- runif( M, min = accMin, max = accMax)
  Y <- rbinom(n = N, size = 1, prob = .5*(1+b))
  
  Fx <- array(data = NA, dim = c(N,M))
  
  for (m in 1:M){
    Fx[,m] <- fxi(psi = Psi[m], eta = Eta[m], Y = Y)
  }
  
  return(list("Fx" = Fx, "Y" = Y, "Eta" = Eta, "Psi" = Psi, "b" = b, "accMin" = accMin, "accMax" = accMax))
  
}


