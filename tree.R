# install.packages('rpart',dependencies=TRUE)
# install.packages('randomForest',dependencies=TRUE)
# install.packages('party')


# Clearing memory, loading packages
rm(list=ls())
library(rpart)
library(e1071)
library(party)
library(randomForest)
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
model.tree <- rpart(ANGLE.CLOSURE~., data = data.tr, method="class")
printcp(model.tree) # display the results 
plotcp(model.tree) # visualize cross-validation results 
summary(model.tree) # detailed summary of splits
pred <- predict(model.tree, data.tr, type="class")
tab <- table(pred=pred, true=data.tr[,21])
classAgreement(tab)

# Visualizing the decision tree
plot(model.tree, uniform=TRUE, main="Classification Tree")
text(model.tree, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
index.min <- which.min(model.tree$cptable[,"xerror"])
model.tree <- prune(model.tree, cp=model.tree$cptable[index.min,"CP"])

# Visualizing the decision tree
plot(model.tree, uniform=TRUE, main="Classification Tree")
text(model.tree, use.n=TRUE, all=TRUE, cex=.8)

# Random Forest prediction of Kyphosis data
model.tree <- randomForest(ANGLE.CLOSURE~., data = data.tr, importance=TRUE, ntree=300)
print(model.tree) # view results 
importance(model.tree) # importance of each predictor

# Random Forest prediction of Kyphosis data
model.tree <- cforest(ANGLE.CLOSURE~., data = data.tr, controls=cforest_unbiased(ntree=300, mtry=3))
pred <- predict(model.tree)
tab <- table(pred=pred, true=data.tr[,21])
classAgreement(tab)
