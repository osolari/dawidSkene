require("dplyr")

dataGen <- function(fileAdd = "/Users/darkGene/workspace/repo/dawidSkene/anesthetic.csv"){
  
  dataStr <- read.table(fileAdd)
  data.df <- dataStr %>% arrange(V1) %>% select(-V1)
  
  obs1 <-  matrix(unlist(lapply(data.df$V2,
                          FUN = function(x){ return(as.numeric(unlist(strsplit(as.character(x), "")))) })), ncol = 3, byrow = T)
  
  dataMat <- array(data = NA, dim = c(45,5,3))
  dataMat[,,1] <- as.matrix(data.df)
  dataMat[,1,] <- obs1

  return(dataMat)
}


dawidSkeneComp <- function(FX){
  
  I = dim(FX)[1]
  J = length(unique(as.vector(FX)))
  K = dim(FX)[2]
  
  N <- array(NA, dim = c(I, J, K)) #
  for (i in 1:I){
    for (j in 1:J){
      for (k in 1:K){
        N[i,j,k] <- sum(FX[i,k]== j-1)
      }
    }
  }
  
  ### Step i: Initial Estimates
  
  Tinit <- matrix(0, nrow = I, ncol = J)
  for (i in 1:I){
    for (j in 1:J){
      Tinit[i,j] <- sum(N[i,j,])/sum(N[i,,])
    }
  }
  
  T_ij <- Tinit
  
  for (iter in 1:20){
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
    
    T_ij <- TijNew
    
  }
  
  binaryPred <- as.integer(T_ij[,2] > .5)
  
  return(list("T"= T_ij, "Pi"= PI, "P" = P, "N" = N, "class" = binaryPred))
  
}