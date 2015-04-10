# Clear memory and load packages
rm(list=ls())
library(stats)
library(glmnet)
library(ggplot2)
library(scatterplot3d)

# Read data, remove columns, clean NA rows
my.data <- read.csv("AngleClosure.csv")
my.data <- my.data[,-c(1,15,16)]
my.data <- my.data[complete.cases(my.data),]
my.data[,-21] <- data.matrix(my.data[,-21])
my.data[,21] <- factor(my.data[,21])
row.names(my.data) <- NULL

# Exploratory analysis of the data
ggplot(my.data, aes(x=ACA, y=ACV, color=ANGLE.CLOSURE)) + 
  geom_point()
# Exploratory analysis of the data: PCA
my.pca <- prcomp(my.data[,-21], scale.=T)
my.pcdata <- as.matrix(my.data[,-21]) %*% my.pca$rotation
my.pcdata <- as.data.frame(my.pcdata)
# Exploratory analysis of the data: PCA in 3DS
scatterplot3d(my.pcdata$PC1, my.pcdata$PC2, my.pcdata$PC3, 
              color=as.numeric(my.data$ANGLE.CLOSURE))

# -------------------- Fitting Logistic Model -------------------- #

# Arrange the data
x <- data.matrix(my.data[,-21])
y <- factor(my.data[,21])

# Fit logistic model, do cross validation
my.logit = glmnet(x, y, family = "binomial", alpha=0.5)
plot(my.logit, xvar = "dev", label = TRUE)
my.cv = cv.glmnet(x, y, family = "binomial", , alpha=0.5, 
                  nfolds=50, type.measure = "class")
plot(my.cv)

# Compute mis-classification rate
my.pred <- predict(my.cv, newx = x, s = "lambda.min", type = "class")
sum(y == my.pred) / length(my.pred)

# Find coefficients
coef(my.cv, s = "lambda.min")

# Compute log loss value
my.pred <- predict(my.cv, newx = x, s = "lambda.min", type = "response")
logloss <- 0
for(i in 1:length(y)) {
  if(y[i]=="YES") logloss <- logloss + log(my.pred[i])
  else logloss <- logloss + log(1-my.pred[i])
}

logloss <- -logloss/length(y)


# -------------------- Fitting Logistic Model -------------------- #

# Fit logistic model
my.logit <- glm(ANGLE.CLOSURE ~., data=my.data, family="binomial")
my.logit <- glm(ANGLE.CLOSURE ~ AOD750 + TISA750 + ACA + ACV + 
                  LENSVAULT + ASPH, data=my.data, family="binomial")
summary(my.logit)
 
# Check Accuracy
my.pred <- predict(my.logit, type="response")
my.pred <- sapply(my.pred, function(p){
  if(p>0.5) return("YES")
  else return("NO")
})
sum(my.pred == my.data[,21]) / length(my.pred)