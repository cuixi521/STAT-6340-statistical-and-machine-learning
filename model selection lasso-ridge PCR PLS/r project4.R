install.packages("ElemStatLearn")
library(ElemStatLearn)
install.packages("car")
library(car)
attach(prostate)
?prostate
summary(prostate)
str(prostate)
prostate$svi<-factor(prostate$svi,level=c('0','1'))
str(prostate)
#take all observation as the training data
prostate$train<-NULL
head(prostate)
plot(prostate,pch=20)
#Linear Model
fit.linear<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostate)
fit.linear
summary.lm(fit.linear)
summary.aov(fit.linear)
fit.linear$coefficients
#cross validation to get error rate
library(boot)
glm.fit <- glm(lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data = prostate)
cv.err <- cv.glm(prostate, glm.fit, K=10)$delta[1]
cv.err
#Best-subset selection
totpred <- ncol(prostate) - 1
library(leaps)
fit.full <- regsubsets(lpsa ~ ., prostate, nvmax = totpred)
fit.summary <- summary(fit.full)
fit.summary
names(fit.summary)
# Cross-validation approach (using best subset selection)
k <- 10 # No. of folds
n<-nrow(prostate)
set.seed(1)
# Split the observations into 10 folds
f<-ceiling(n/k)
#folds <- sample(rep(1:k, length.out=nrow(prostate)), nrow(prostate)) 
folds<-sample(rep(1:k,f), n)
# Create a k x totpred matrix to store test errors for 
# each fold number and model size combination
cv.errors <- matrix(NA, k, totpred, dimnames = list(NULL, paste(1:totpred)))

# Write a function to easily get predictions for a model 
# from a regsubsets object
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

for (j in 1:k) {
  # Best subset selection on the training folds
  best.fit <- regsubsets(lpsa ~ ., data = prostate[folds != j, 
                                                   ], nvmax = totpred)
  # Prediction on the test fold
  for (i in 1:totpred) {
    # Using the predict.regsubsets function written above
    pred <- predict(best.fit, prostate[folds == j, ], id = i)
    cv.errors[j, i] = mean((prostate$lpsa[folds == j] - pred)^2)
  }
}

# Get the mean for each column (model size)

mean.cv.errors <- apply(cv.errors, 2, mean)

# Plot cv errors against model size 
par(mfrow = c(1, 2))
plot(mean.cv.errors,type="b")
which.min(mean.cv.errors)
#nbest=3
fit.best<-regsubsets(lpsa ~ ., prostate, nbest = 3)
?plot.regsubsets
plot(fit.full, scale = "r2")
plot(fit.full, scale = "adjr2")
plot(fit.full, scale = "Cp")
plot(fit.full, scale = "bic")

# Get coefficients of best model for a given size
coef(fit.full, 3)
#error rate
err.bestmodel<-min(mean.cv.errors)
err.bestmodel
# Ridge Regression #
y <- prostate$lpsa
x <- model.matrix(lpsa ~ ., prostate)[, -1]
install.packages("glmnet")
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
# Fit ridge regression for each lambda on the grid
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
plot(ridge.mod, xvar = "lambda")
# Use 10 fold cross-validation to estimate test MSE from training data
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 0)
?cv.glmnet
#cv.out
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
# Refit the model on the full dataset 
out <- glmnet(x, y, alpha = 0, lambda=bestlam)
ridge.coef <- predict(out, type = "coefficients", s = bestlam)
ridge.coef
# Test MSE for the best value of lambda
err.ridge<-min(cv.out$cvm)
err.ridge
#ridge.pred <- predict(out, s = bestlam, newx = x)
#mean((ridge.pred - y)^2)

# Lasso #
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.mod, xvar = "lambda")
# Use cross-validation to estimate test MSE using training data
set.seed(1)
?cv.glmnet
cv.out <- cv.glmnet(x, y, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
#cv.out$cvm
bestlam
out <- glmnet(x, y, alpha = 1, lambda = bestlam)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef
#error rate
mErr.lasso<-min(cv.out$cvm)
mErr.lasso

#PCR
install.packages('pls')
library(pls)
set.seed(2)
pcr.fit <- pcr(lpsa ~ ., data = prostate, scale = TRUE, validation = "CV", segments = 10)
#results
summary(pcr.fit)
#get MSE
MSEP(pcr.fit)
sqrt(MSEP(pcr.fit)$val[1, 1,])
which.min(MSEP(pcr.fit)$val[1, 1,])
#plot the cross-validation test MSE estimates
validationplot(pcr.fit, val.type = "MSEP")
# We see that lowest cross-validation error occurs when M = 8 -->
# Compute test MSE for M = 8
err.pcr<-min(MSEP(pcr.fit)$val[1,1,])
err.pcr
#fit the model with all the data
pcr.fit <- pcr(lpsa ~ ., data = prostate, scale = TRUE, ncomp = 8)
summary(pcr.fit)
pcr.fit$coefficients
coef(pcr.fit, intercept=T)

# PLS # 
# Fit PLS on training data
set.seed(1)
pls.fit <- plsr(lpsa ~ ., data = prostate, scale = TRUE, validation = "CV", segments = 10)
validationplot(pls.fit, val.type = "MSEP")
summary(pls.fit)
# We see that lowest cross-validation error occurs when M = 5 -->
# Compute test MSE for M = 5 -->
#test error rate for pls
err.pls<-min(MSEP(pls.fit)$val[1,1,])
err.pls
# This result is slightly higher than those for PCR and lasso -->
# Refit the model on the full dataset  -->
pls.fit <- plsr(lpsa ~ ., data = prostate, scale = TRUE, ncomp = 5)
summary(pls.fit)
#Look at results
coef(pls.fit, intercept=T)


