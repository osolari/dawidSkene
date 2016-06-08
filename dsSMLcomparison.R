setwd("~/workspace/repo/dawidSkene/")
require("dplyr")
require("MASS")
require("ggplot2")
source("./dsSMLcomparison.lib.R")
source("dawidSkene.lib.R")
require("pROC")


simData <- simResponseData(N = 1000)
dsOut <- dawidSkeneComp(FX = simData$Fx)

PsiSeq <- runif(n = 10, min = .5, max = .8)
EtaSeq <- runif(n = 10, min = .5, max = .8)
PiSeq <- (PsiSeq + EtaSeq)/2


b <- c(0,.3,.6)
nSamp <- c(100,1000,10000)
nIter <- 20
classImbalance <- array(data = NA, dim = c(length(b),nIter,length(nSamp)))
accuraciesMSE <- array(data = NA, dim = c(length(b),nIter,length(nSamp)))
errorRate <- array(data = NA, dim = c(length(b),nIter,length(nSamp)))
balancedAccuracy <- array(data = NA, dim = c(length(b),nIter,length(nSamp)))
for (i in 1:length(b)){
  for (j in 1:nIter){
    for (k in 1:length(nSamp)){
      simData <- simResponseData(N = nSamp[k], b = b[i], Psi = PsiSeq, Eta = EtaSeq)
      dsOut <- dawidSkeneComp(FX = simData$Fx)
      classImbalance[i,j,k] <- diff(dsOut$P)
      accuraciesMSE[i,j,k] <- sum(((dsOut$Pi[1,1,] + dsOut$Pi[2,2,])/2 - PiSeq)^2)
      errorRate[i,j,k] <- sum(!(dsOut$class == simData$Y))/length(simData$Y)
      balancedAccuracy[i,j,k] <- sum(!(dsOut$class + simData$Y == 1))/length(simData$Y)
    }
  }
}

b.df <- t(data.frame(classImbalance))
colnames(b.df) <- c("0",".3", ".6")
require("reshape2")
b.df.melt <- melt(b.df)
b.df.melt$N <- rep(x = nSamp, each = nIter)
b.df.melt$Var2 <- as.factor(b.df.melt$Var2)

pdf("classImbalance3.pdf", width = 10, height = 10)
ggplot(data = b.df.melt) + geom_point(aes(x = N, y = value, col = Var2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text = element_text(size = 20)) +
  scale_color_manual(values = c("red","black","blue"), name = "true class imbalance") +
  labs(x = "N", y = "Estimated Class Imbalance") +
  scale_x_log10() + geom_hline(yintercept = 0, col = "red", linetype="dashed") +
  geom_hline(yintercept = 0.3, col = "black", linetype="dashed") +
  geom_hline(yintercept = 0.6, col = "blue", linetype="dashed")
  
dev.off()

############

p.df <- t(data.frame(accuraciesMSE))
colnames(p.df) <- c("0",".3", ".6")
require("reshape2")
p.df.melt <- melt(p.df)
p.df.melt$N <- rep(x = nSamp, each = nIter)
p.df.melt$Var2 <- as.factor(p.df.melt$Var2)

pdf("mse.pdf", width = 10, height = 10)
ggplot(data = p.df.melt, aes(x = as.factor(N), y = value, alpha = .4)) + geom_boxplot() + geom_jitter() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text = element_text(size = 20), axis.title = element_text(size = 20))+
  labs(x = "N", y = "MSE of Estimated Balanced Accuracies of All Classifiers") +
  scale_alpha(guide = "none")

dev.off()

###########

t.df <- t(data.frame(errorRate))
colnames(t.df) <- c("0",".3", ".6")
require("reshape2")
t.df.melt <- melt(t.df)
t.df.melt$N <- rep(x = nSamp, each = nIter)
t.df.melt$Var2 <- as.factor(t.df.melt$Var2)

pdf("err.pdf", width = 10, height = 10)
ggplot(data = t.df.melt, aes(x = as.factor(N), y = value, alpha = .4)) + geom_boxplot() + geom_jitter() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text = element_text(size = 20), axis.title = element_text(size = 20))+
  labs(x = "N", y = "Error Rate of The Ensemble Classifier") +
  scale_alpha(guide = "none")

dev.off()

########
a.df <- t(data.frame(balancedAccuracy))
colnames(a.df) <- c("0",".3", ".6")
require("reshape2")
a.df.melt <- melt(a.df)
a.df.melt$N <- rep(x = nSamp, each = nIter)
a.df.melt$Var2 <- as.factor(a.df.melt$Var2)

PiSeq.df <- data.frame("Accuracies" =rep(x = PiSeq, times = 3)) 
PiSeq.df$N <- rep(nSamp, each = length(PiSeq))

pdf("BalancedAcc.pdf", width = 10, height = 10)
ggplot() + geom_boxplot(data = a.df.melt, aes(x = as.factor(N), y = value, alpha = .4)) + geom_jitter(data = a.df.melt, aes(x = as.factor(N), y = value, alpha = .4, col = "black")) +
  geom_jitter(data = PiSeq.df, aes(x = as.factor(N), y = Accuracies, col = "red")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text = element_text(size = 20), axis.title = element_text(size = 20))+
  labs(x = "N", y = "Balanced Accuracy") +
  scale_color_manual(name = "classifier", values = c("black","red"), labels = c("ensemble classifier", "individual classifiers") )+
  scale_alpha(guide = "none")

dev.off()


