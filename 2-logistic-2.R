# -------------------- Testing model -------------------- #

y.tr <- data.tr[,1]
x.tr <- data.matrix(data.tr[,-1])
y.te <- data.te[,1]
x.te <- data.matrix(data.te[,-1])

# Fitting the final model, getting the prediction and testing the model
model <- glmnet(x=x.tr, y=y.tr, family = "binomial", alpha=0, lambda=0.2482957578)
dput(model, "0-model-logistic.R")

> model$beta
11 x 1 sparse Matrix of class "dgCMatrix"
s0
AOD750    -0.3185054961
TISA750   -0.6526404782
IT750      0.1709290159
IT2000     0.0987614727
ITCM       0.1401309094
IAREA      0.0077074521
ICURV      0.3179336602
ACW_mm    -0.0698256028
ACA       -0.0181303345
ACV       -0.0020997805
LENSVAULT  0.0002093944

yHat <- predict(model, newx=x.te, type="response")
roc <- roc(data.te[,1]~yHat, data.te, plot=T)

> roc$auc
Area under the curve: 0.9983