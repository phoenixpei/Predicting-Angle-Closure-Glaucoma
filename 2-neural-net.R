# Clearing memory, loading package, reading the data
rm(list=ls())
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw
           /466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
source_url('https://gist.github.com/fawda123/6206737/raw
           /2e1bc9cbc48d1a56d2a79dd1d33f414213f5f1b1/gar_fun.r')
library(nnet)
library(devtools)
library(pROC)
library(lattice)
data.tr <- dget("0-training-data.r")
data.te <- dget("0-testing-data.r")

# -------------------- Choosing size and decay -------------------- #

# Initializing values
nIter <- 50
size <- seq(6,27,3)
decay <- seq(0.5,6.0,0.5)
#   seq(0.2,2.0,0.2)
my.auc <- array(0, dim=c(nIter,length(size),length(decay)))

for(iter in 1:nIter){
  # Initializing the cross validation
  data.index <- sample(nrow(data.tr))[1:round(nrow(data.tr)/10)]
  for(s in size) { for(d in decay) {
    model <- nnet(y~., data=data.tr[-data.index,], size=s, decay=d)
    yHat <- predict(model, data.tr[data.index,], type="raw")
    roc <- roc(data.tr[data.index,1]~yHat, data.tr[data.index,])
    my.auc[iter,which(size==s),which(decay==d)] <- roc$auc
  }}
  print(iter)
}
levelplot(apply(my.auc,c(2,3),mean))

# Fitting the final model
model  <- nnet(y~., data=data.tr, size=6, decay=0.5)
plot.nnet(model)
# Getting the prediction
yHat <- predict(model, data.te, type="raw")
# Computing the ROC and AUC
roc <- roc(data.te[,1]~yHat, data.te, plot=T)

> roc$auc
Area under the curve: 0.9287