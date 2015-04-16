# Clearing memory, loading package, reading the data
rm(list=ls())
library(ada)
library(pROC)
library(ggplot2)
data.tr <- dget("0-training-data.r")
data.te <- dget("0-testing-data.r")

# -------------------- Choosing value of nu -------------------- #

# Initializing the parameter tuning
nIter <- 10
nu <- 10^seq(-2,0.4,0.2)
auc <- matrix(NA, nIter, length(nu))

for(iter in 1:nIter){
  # Initializing the cross validation
  data.index <- sample(nrow(data.tr))[1:round(nrow(data.tr)/10)]
  for(u in nu){
    model  <- ada(y~., data=data.tr[-data.index,], loss="logistic", type="discrete", iter=50, nu=u)
    yHat <- predict(model, data.tr[data.index,], type="prob")
    roc <- roc(data.tr[data.index,1]~yHat[,1], data.tr)
    auc[iter,which(nu==u)] <- auc(roc)
  }
  print(iter)
}
data.plot <- data.frame(nu, apply(auc,2,mean))
names(data.plot) <- c("nu", "average.auc")
ggplot(data.plot, aes(x=log(nu), y=average.auc)) + 
  geom_point(shape=1) +  geom_smooth()

# -------------------- Testing model -------------------- #

# Fitting the final model, getting the prediction and testing the model
model  <- ada(y~., data=data.tr, loss="logistic", type="discrete", iter=50, nu=0.0631)
# dput(model, "0-model-adaboost.R")
yHat <- predict(model, data.te, type="prob")
roc <- roc(data.te[,1]~yHat[,1], data.te, plot=T)
auc(roc)