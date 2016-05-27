setwd("~/workspace/repo/dawidSkene/")
require("dplyr")
require("MASS")
source("./dsSMLcomparison.lib.R")
source("dawidSkene.lib.R")



simData <- simResponseData(N = 1000)
dsOut <- dawidSkeneComp(FX = simData$Fx)

b <- c(.3,.6)
nSamp <- c(100,500,1000)
nIter <- 10
classImbalance <- array(data = NA, dim = c(length(b),nIter,length(nSamp)))
for (i in 1:length(b)){
  for (j in 1:nIter){
    for (k in 1:length(nSamp)){
      simData <- simResponseData(N = nSamp[k], b = b[i])
      dsOut <- dawidSkeneComp(FX = simData$Fx)
      classImbalance[i,j,k] <- diff(dsOut$P)
    }
  }
}

b.df <- t(data.frame(classImbalance))
colnames(b.df) <- c(".3", ".6")
require("reshape2")
b.df.melt <- melt(b.df)
b.df.melt$N <- rep(x = nSamp, each = 10)
b.df.melt$Var2 <- as.factor(b.df.melt$Var2)

pdf("classImbalance.pdf", width = 10, height = 10)
ggplot(data = b.df.melt) + geom_point(aes(x = N, y = value, col = Var2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text = element_text(size = 20)) +
  scale_color_manual(values = c("red","black"), name = "true class imbalance") +
  labs(x = "N", y = "Estimated Class Imbalance")
  
dev.off()

