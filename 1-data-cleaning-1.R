# Clearing memory, reading the data
rm(list=ls())
data.tr <- read.csv("AngleClosure.csv")

# Cleaning the data
data.tr <- data.tr[complete.cases(data.tr),]
data.tr <- data.tr[,-c(1,15,16)]
data.tr[,-21] <- data.matrix(data.tr[,-21])
data.tr[,21] <- factor(data.tr[,21])
row.names(data.tr) <- NULL

# Omiting the rows, storing the data
y <- data.tr[,21]
data.tr <- data.tr[,1:11]
data.tr <- cbind(y, data.tr)
dput(data.tr, "0-training-data.r")