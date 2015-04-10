# install.packages('e1071',dependencies=TRUE)

# Clearing memory, loading packages
rm(list=ls())
library(e1071)
# Reading the data
data.tr <- read.csv("AngleClosure.csv")
data.ca <- read.csv("AngleClosure_ValidationCases.csv")
data.co <- read.csv("AngleClosure_ValidationControls.csv")
# Cleaning the data
data.tr <- data.tr[complete.cases(data.tr),]
data.tr <- data.tr[,-c(1,15,16)]
data.tr[,-21] <- data.matrix(data.tr[,-21])
data.tr[,21] <- factor(data.tr[,21])
row.names(data.tr) <- NULL

# Fitting the model
model.svm  <- svm(ANGLE.CLOSURE~., data = data.tr)
summary(model.svm)
# Checking its accuracy
pred <- predict(model.svm, data.tr)
tab <- table(pred=pred, true=data.tr[,21])
classAgreement(tab)
tuned <- tune.svm(ANGLE.CLOSURE~., data = data.tr)
# Error estimation of ‘svm’ using 10-fold cross validation: 0.09985719

niter <- 5
cost <- rep(0,niter)
gamma <- rep(0,niter)
para.gamma <- 1e-05
for(i in 1:niter) {
  # Adjusting the model
  tuned <- tune.svm(ANGLE.CLOSURE~., data = data.tr, gamma = para.gamma, cost = 10^seq(3,5,0.1))
  cost[i] <- para.cost <- tuned$best.parameters$cost
  # Adjusting the model
  tuned <- tune.svm(ANGLE.CLOSURE~., data = data.tr, gamma = 10^seq(-6,-4,0.1), cost = para.cost)
  gamma[i] <- para.gamma <- tuned$best.parameters$gamma
}

para.gamma <- 1.258925e-06
para.cost <- 10000.000
# Fitting the model
model.svm  <- svm(ANGLE.CLOSURE~., data = data.tr, gamma = para.gamma, cost = para.cost)
summary(model.svm)
# Checking its accuracy
pred <- predict(model.svm, data.tr)
tab <- table(pred=pred, true=data.tr[,21])
classAgreement(tab)
tuned <- tune.svm(ANGLE.CLOSURE~., data = data.tr,gamma = para.gamma, cost = para.cost)
# Error estimation of ‘svm’ using 10-fold cross validation: 0.08526394