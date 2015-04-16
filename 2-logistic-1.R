# Clearing memory, loading package, reading the data
rm(list=ls())
library(glmnet)
library(pROC)
library(ggplot2)
data.tr <- dget("0-training-data.r")
data.te <- dget("0-testing-data.r")

# -------------------- Choosing value of lambda -------------------- #

# Initializing the parameter tuning
nIter <- 10
# Doing variable selection using lasso, get lambdas
y.tr <- data.tr[,1]
x.tr <- data.matrix(data.tr[,-1])
model <- glmnet(x=x.tr, y=y.tr, family = "binomial", alpha=1)
lambda <- model$lambda
auc <- matrix(NA, nIter, length(lambda))

# Using ridge regression to tune the parameter
for(iter in 1:nIter){
  # Initializing the cross validation
  data.index <- sample(nrow(data.tr))[1:round(nrow(data.tr)/10)]
  y.tr <- data.tr[-data.index,1]
  x.tr <- data.matrix(data.tr[-data.index,-1])
  y.te <- data.tr[data.index,1]
  x.te <- data.matrix(data.tr[data.index,-1])
  for(l in lambda){
    model <- glmnet(x=x.tr, y=y.tr, family = "binomial", alpha=0, lambda=l)
    yHat <- predict(model, newx=x.te, type="response")
    roc <- roc(data.tr[data.index,1]~yHat, data.tr[-data.index,])
    roc$auc
    auc[iter,which(lambda==l)] <- roc$auc
  }
  print(iter)
}
data.plot <- data.frame(lambda, apply(auc,2,mean))
names(data.plot) <- c("lambda", "average.auc")
ggplot(data.plot, aes(x=log(lambda), y=average.auc)) + 
  geom_point(shape=1) +  geom_smooth()