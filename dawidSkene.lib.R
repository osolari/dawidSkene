require("dplyr")

dataGen <- function(fileAdd = "/Users/darkGene/workspace/repo/ensemble/anesthetic.csv"){
  
  dataStr <- read.table(fileAdd)
  data.df <- dataStr %>% arrange(V1) %>% select(-V1)
  
  obs1 <-  matrix(unlist(lapply(data.df$V2,
                          FUN = function(x){ return(as.numeric(unlist(strsplit(as.character(x), "")))) })), ncol = 3, byrow = T)
  
  dataMat <- array(data = NA, dim = c(45,5,3))
  dataMat[,,1] <- as.matrix(data.df)
  dataMat[,1,] <- obs1

  return(dataMat)
}
