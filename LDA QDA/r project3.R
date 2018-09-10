install.packages("ElemStatLearn")
install.packages("crossval")
library(crossval)
library(ElemStatLearn)

#data prepared
attach(SAheart)
summary(SAheart)
SAheart$chd<-as.factor(SAheart$chd)

#1.a
fit.full<-glm(chd~sbp+tobacco+ldl+adiposity+famhist+typea+obesity+alcohol+age, family = binomial, data = SAheart)
summary.glm(fit.full)
fit.re<-glm(chd~tobacco+ldl+famhist+typea+age,family=binomial,data=SAheart)
#hypothesis test
anova(fit.re,fit.full,test = "Chisq") #Note: The dropped predictors are not significant
#coefficients
summary.glm(fit.re)

#b need picrues I'll add later

#logistic regression model
fit<-glm(chd~tobacco+ldl+typea+age,family=binomial,data=SAheart)
lr.prob <- predict(fit,SAheart, type = "response")
#classification
lr.pred <- ifelse(lr.prob >= 0.5, "1", "0")
#confusion matrix
table(lr.pred, SAheart[, "chd"])
#sensitivity and specificity
cm<-matrix(table(lr.pred, SAheart[, "chd"]),nrow=2,ncol = 2,byrow=FALSE)
c(cm[2,2]/sum(cm[,2]),cm[1,1]/sum(cm[,1]))
#overall misclassification rate
1 - mean(lr.pred == SAheart[, "chd"])
#ROC curve
install.packages('pROC')
library(pROC)
roc.lr <- roc(SAheart[, "chd"], lr.prob, levels = c("0", "1"))
plot(roc.lr, legacy.axes = T)

#c,d,e
train.X<-SAheart[,c(2,3,6,9)]
train.y<-SAheart[,c(10)]
#LDA
#find decision boundary
s6340.lda <- function(y, X) {
  # y = training data response vector (a factor)
  # X = training data predictor matrix 
  N <- length(y) # no of observations
  K <- nlevels(y) # no of classes
  p <- ncol(X) # no of predictors
  n <- as.numeric(table(y)) # class frequencies
  names(n) <- levels(y)
  pi <- n/N # class proportions
  # mean vector
  mu <- matrix(unlist(by(X, y, colMeans)), byrow = T, ncol = p)
  rownames(mu) <- levels(y)
  colnames(mu) <- colnames(X)
  # pooled covariance matrix
  S <- by(X, y, cov)
  Sigma <- Reduce("+", lapply(1:K, FUN = function(k) {(n[k] - 1) * S[[k]]}))/(N - K)
  # its inverse
  Sigma.inv <- solve(Sigma)
  # delta functions
  delta <- t(sapply(1:K, FUN = function(k) {
    c(-(1/2) * drop(t(mu[k, ]) %*% Sigma.inv %*% mu[k, ]) + 
        log(pi[k]), t(mu[k, ]) %*% Sigma.inv)}))
  rownames(delta) <- levels(y)
  colnames(delta) <- c("(Intercept)", colnames(X))
  # pairwise difference of delta functions
  idx.pair <- combn(K, 2)
  delta.diff <- t(apply(idx.pair, MAR = 2, FUN = function(pair) {
    delta[pair[1], ] - delta[pair[2], ]}))
  rownames(delta.diff) <- apply(idx.pair, MAR = 2, FUN = function(pair) {
    paste0(levels(y)[pair[1]], "-", levels(y)[pair[2]])})
  # multiply intecept difference by 1 to get the cutoff c	
  delta.diff[, 1] <- -delta.diff[, 1]
  colnames(delta.diff)[1] <- "Cutoff"
  # result
  result <- list(N = N, n = n, pi = pi, mu = mu, Sigma = Sigma, 
                 delta = delta, disc = delta.diff)
  return(result)
}
our.lda.fit <- s6340.lda(train.y,train.X)
our.lda.fit$disc

#fit by package
library(MASS)
lda.fit <- lda(chd~tobacco+ldl+typea+age,data=SAheart)
lda.pred<- predict(lda.fit, SAheart)
#fit by function
train.X<-as.matrix(as.data.frame(train.X))
score.test <- train.X %*% coeff
#predict class
pred.test <- ifelse(score.test >= cutoff, "0", "1")
#confusion matrix
table(pred.test, train.y)
#sensitivity and specificity
cm<-matrix(table(pred.test, train.y),nrow=2,ncol = 2,byrow=FALSE)
c(cm[2,2]/sum(cm[,2]),cm[1,1]/sum(cm[,1]))
#overall misclassification rate
1 - mean(pred.test == SAheart[, "chd"])
#ROC curve
lda.prob<-lda.pred$posterior[,2]
roc.lda <- roc(SAheart[, "chd"], lda.prob, levels = c("0", "1"))
plot(roc.lda, legacy.axes = T)

#QDA 
#find decision boundary
s6340.qda <- function(y, X) {
  # y = training data response vector (a factor)
  # X = training data predictor matrix 
  N <- length(y) # no of observations
  K <- nlevels(y) # no of classes
  p <- ncol(X) # no of predictors
  n <- as.numeric(table(y)) # class frequencies
  names(n) <- levels(y)
  pi <- n/N # class proportions
  # mean vector
  mu <- matrix(unlist(by(X, y, colMeans)), byrow = T, ncol = p)
  rownames(mu) <- levels(y)
  colnames(mu) <- colnames(X)
  #covariance matrix 
  S <- by(X, y, cov)
  # its inverse
  Sigma.inv <- lapply(1:K, FUN=function(k){solve(S[[k]])})
  # delta functions
  delta <- t(sapply(1:K, FUN = function(k) {c(-(1/2) * drop(t(mu[k, ]) %*% Sigma.inv[[k]] %*% mu[k, ]) + 
        log(pi[k])-(1/2)*log(drop(det(S[[k]]))),drop(t(mu[k, ]) %*% Sigma.inv[[k]]),-(1/2)*drop(Sigma.inv[[k]]))}))
  
  rownames(delta) <- levels(y)
  #colnames(delta) <- c("(Intercept)", colnames(X), "Sigma")
  # pairwise difference of delta functions
  idx.pair <- combn(K, 2)
  delta.diff <- t(apply(idx.pair, MAR = 2, FUN = function(pair) {
    delta[pair[1], ] - delta[pair[2], ]}))
  #rownames(delta.diff) <- apply(idx.pair, MAR = 2, FUN = function(pair) {
  #  paste0(levels(y)[pair[1]], "-", levels(y)[pair[2]])})
  # multiply intecept difference by 1 to get the cutoff c	
  delta.diff[, 1] <- -delta.diff[, 1]
  colnames(delta.diff)[1] <- "say"
  # result
  result <- list(N = N, n = n, pi = pi, mu = mu, Sigma = S, 
                 delta = delta,disc = delta.diff)
  return(result)
}
our.qda.fit <- s6340.qda(train.y, train.X)
our.qda.fit$disc[,c(1:5)]
sigma1_sigma2<-matrix(our.qda.fit$disc[,c(6:21)],nrow=4,ncol=4,byrow = TRUE)
sigma1_sigma2

#fit qda model and predict class
qda.fit <- qda(chd~tobacco+ldl+typea+age,data=SAheart)
qda.pred<- predict(qda.fit, SAheart)
#confusion matrix
table(qda.pred$class, SAheart[, "chd"])
#sensitivity and specificity
cm<-matrix(table(qda.pred$class, SAheart[, "chd"]),nrow=2,ncol = 2,byrow=FALSE)
c(cm[2,2]/sum(cm[,2]),cm[1,1]/sum(cm[,1]))
#overall misclassification rate
1 - mean(qda.pred$class == SAheart[, "chd"])
#ROC curve
qda.prob<-qda.pred$posterior[,2]
roc.lqda <- roc(SAheart[, "chd"], qda.prob, levels = c("0", "1"))
plot(roc.qda, legacy.axes = T)




