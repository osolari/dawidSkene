setwd("~/workspace/repo/dawidSkene/")
require("dplyr")
require("ggplot2")
source("dawidSkene.lib.R")

f <- dataGen()
#f <- f[1:5,1:2,1:2]
I = dim(f)[1]
J = sum(!is.na(unique(unlist(apply(X = f, MARGIN = 2, unique)), na.rm = T)))
K = dim(f)[2]


# Tested N
N <- array(NA, dim = c(I, J, K)) #
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

for (iter in 1:40){
### Step ii: Estimate PI_ij and p_j #checked
  PI <- array(data = NA, dim = c(J,J,K))
  for (k in 1:K){
    for (j in 1:J){
      for (l in 1:J){
        PI[j,l,k] <-  T_ij[,j] %*% N[,l,k] / sum(T_ij[,j] %*% N[,,k])
      }
    }
  }


  P <- array(data = NA, dim = J) #Check
  P <- colSums(T_ij)/I #Check

### Step iii: estimating new true labels
  TijNew <- array(data = 1, dim = c(I,J))
  for (i in 1:I){
    for (k in 1:K){
      for (l in 1:J){
        TijNew[i,] <- TijNew[i,] * PI[,l,k] ^ N[i,l,k]
      }
    }
    TijNew[i,] <- (TijNew[i,] * P)/sum(TijNew[i,] * P)
  }
  
  
#   TijNew <- array(data = 1, dim = c(I,J))
#   for (i in 1:I){
#     for (k in 1:K){
#       for (l in 1:J){
#         TijNew[i,] <- TijNew[i,] * PI[,l,k] ^ N[i,l,k] * P
#       }
#     }
#     TijNew[i,] <- TijNew[i,]/sum(TijNew[i,])
#   }

  T_ij <- TijNew

}

