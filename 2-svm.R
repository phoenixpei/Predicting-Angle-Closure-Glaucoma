# Clearing memory, loading package, reading the data
rm(list=ls())
library(e1071)
library(pROC)
library(lattice)
data.tr <- dget("0-training-data.r")
data.te <- dget("0-testing-data.r")

# Initializing values
nIter <- 10
gamma <- 10^seq(-9,0,0.2)
cost <- 10^seq(-9,0,0.25)
my.auc <- array(0, dim=c(nIter,length(gamma),length(cost)))

# -------------------- Choosing gamma and cost -------------------- #

for(iter in 1:nIter){
  # Initializing the cross validation
  data.index <- sample(nrow(data.tr))[1:round(nrow(data.tr)/10)]
  for(g in gamma) { for(c in cost) {
    model  <- svm(y~., data=data.tr[-data.index,], gamma=g, cost=c, probability=TRUE)
    yHat <- predict(model, data.tr[data.index,], probability=TRUE)
    yHat <- attr(yHat, "probabilities")
    roc <- roc(data.tr[data.index,1]~yHat[,1], data.tr[data.index,])
    my.auc[iter,which(gamma==g),which(cost==c)] <- roc$auc
  }}
  print(iter)
}
test <- apply(my.auc,c(2,3),mean)
levelplot(test)

# -------------------- Testing model -------------------- #

# Fitting the final model
model  <- svm(y~., data=data.tr, gamma=0.0001, cost=1, probability=TRUE)
dput(model, "0-model-svm.R")
# Getting the prediction
yHat <- predict(model, data.te, probability=TRUE)
yHat <- attr(yHat, "probabilities")
# Computing the ROC and AUC
roc <- roc(data.te[,1]~yHat[,1], data.te, plot=T)

> roc$auc
Area under the curve: 0.9983