# Clearing memory, loading package, reading the data
rm(list=ls())
library(randomForest)
library(pROC)
library(ggplot2)
data.tr <- dget("0-training-data.r")
data.te <- dget("0-testing-data.r")

# -------------------- Choosing number of trees -------------------- #

# Initializing the parameter tuning
nIter <- 100
nTree <- seq(30,1200,30)
auc <- matrix(NA, nIter, length(nTree))

for(iter in 1:nIter){
  # Initializing the cross validation
  data.index <- sample(nrow(data.tr))[1:round(nrow(data.tr)/10)]
  for(nt in nTree){
    model  <- randomForest(y~., data=data.tr[-data.index,], ntree=nt)
    yHat <- predict(model, data.tr[data.index,], type="prob")
    roc <- roc(data.tr[data.index,1]~yHat[,1], data.tr)
    auc[iter,which(nTree==nt)] <- auc(roc)
  }
  print(iter)
}
data.plot <- data.frame(nTree, apply(auc,2,mean))
names(data.plot) <- c("number.tree", "average.auc")
ggplot(data.plot, aes(x=number.tree, y=average.auc)) + 
  geom_point(shape=1) +  geom_smooth()

# -------------------- Testing model -------------------- #

# Fitting the final model, getting the prediction and testing the model
model  <- randomForest(y~., data=data.tr, ntree=500)
dput(model, "0-model-rforest.R")
yHat <- predict(model, data.te, type="prob")
roc <- roc(data.te[,1]~yHat[,1], data.te, plot=T)
> auc(roc)
Area under the curve: 0.982