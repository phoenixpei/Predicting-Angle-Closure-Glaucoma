# Clear memory and load packages
rm(list=ls())
library(nnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137
           /raw/466c1474d0a505ff044412703516c34f1a4684a5
           /nnet_plot_update.r')
source_url('https://gist.github.com/fawda123/6206737/raw
           /2e1bc9cbc48d1a56d2a79dd1d33f414213f5f1b1/gar_fun.r')



# Read data, remove columns, clean NA rows
my.data <- read.csv("AngleClosure.csv")
my.data <- my.data[,-c(1,15,16)]
my.data <- my.data[complete.cases(my.data),]
my.data[,-21] <- data.matrix(my.data[,-21])
my.data[,21] <- factor(my.data[,21])
row.names(my.data) <- NULL

accuracy <- rep(0,30)

for(i in 1:30) {
  fit <- nnet(ANGLE.CLOSURE ~., data=my.data, size=i)
  my.pred <- predict(fit, my.data, type = "class")
  accuracy[i] <- sum(my.pred == my.data[,21]) / length(my.pred)
}
plot(seq(30),accuracy)

my.decay <- seq(-2,3,0.17)
my.decay <- 10 ^ my.decay
accuracy <- rep(0,30)

for(i in 1:30) {
  fit <- nnet(ANGLE.CLOSURE ~., data=my.data, decay=my.decay[i], size=20)
  my.pred <- predict(fit, my.data, type = "class")
  accuracy[i] <- sum(my.pred == my.data[,21]) / length(my.pred)
}
plot(seq(30),accuracy)
my.decay[16]



fit <- nnet(ANGLE.CLOSURE ~., data=my.data, decay=3.5, size=20)
my.pred <- predict(fit, my.data, type = "class")
sum(my.pred == my.data[,21]) / length(my.pred)



plot.nnet(fit)






# Read handwritten digits data
myData=read.csv("semeion.csv",header=FALSE)
# Build data matrix with (thresholded) pixel and label data
myX=data.matrix(myData[,1:256])
myLabel=factor(apply(myData[,257:266],1,function(xx){
  return(which(xx=="1")-1)
}))
dataStar=data.frame(myLabel,myX)
library(nnet)
nFolds=10

nIter=25
myLambdas=10^seq(-1,5,1)
ErrRates=matrix(NA,nIter,length(myLambdas))
for(iter in 1:nIter){
  testingIndices=sample(length(myLabel))[1:round(length(myLabel)/nFolds)]
  myDataTesting=dataStar[testingIndices,]
  myDataTraining=dataStar[-testingIndices,]
  for(lambda in myLambdas){
    fit=nnet(myLabel~.,data=myDataTraining,weights=rep(1,length(myLabel)),size=10,
             decay=lambda,MaxNWts=10000,maxit=250)
    myPreds=apply(predict(fit,newdata=myDataTesting),1,which.max)-1
    ErrRates[iter,myLambdas==lambda]=mean(myDataTesting$myLabel!=myPreds)
  }
}
apply(ErrRates,2,mean)
