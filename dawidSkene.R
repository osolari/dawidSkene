setwd("~/workspace/repo/ensemble/")
require("dplyr")
require("ggplot2")
source("dawidSkene.lib.R")

f <- dataGen()
I = dim(f)[1]
J = 4
K = dim(f)[2]

N <- array(NA, dim = c(I, J, K))
for (i in 1:I){
  for (j in 1:J){
    for (k in 1:K){
      N[i,j,k] <- sum(f[i,k,]== j, na.rm = T)
    }
  }
}

### Step i: Initial Estimates

Tinit <- matrix(0, nrow = dim(f)[1], ncol = J)
for (i in 1:I){
  for (j in 1:J){
    Tinit[i,j] <- sum(N[i,j,])/sum(N[i,,])
  }
}

T_ij <- Tinit
# for (i in 1:I){
#   tmp <- f[i,,]
#   for (j in 1:J){
#     Tinit[i,j] <- sum(tmp==j, na.rm = T)/sum(!is.na(tmp))
#   }
# }

for (iter in 1:40){
### Step ii: Estimate PI_ij and p_j
  PI <- array(data = NA, dim = c(J,J,K))
  for (k in 1:K){
    for (j in 1:J){
      for (l in 1:J){
        PI[j,l,k] <-  T_ij[,j] %*% N[,l,k] / sum(T_ij[,j] %*% N[,,k])
      }
    }
  }


  P <- array(data = NA, dim = J)
  P <- colSums(T_ij)/I

### Step iii: estimating new true labels
  TijNew <- array(data = 1, dim = c(I,J))
  for (i in 1:I){
    for (k in 1:K){
      for (l in 1:J){
        TijNew[i,] <- TijNew[i,] * PI[,l,k] ^ N[i,l,k] * P
      }
    }
    TijNew[i,] <- TijNew[i,]/sum(TijNew[i,])
  }

  T_ij <- TijNew

}

