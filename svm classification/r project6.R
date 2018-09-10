install.packages("e1071")
library(e1071)
library(ISLR)
#--------------a-------------
#data preparing
attach(OJ)
data<-OJ
train<-data[1:870,]
test<-data[871:1070,]
#--------------b------------
set.seed(1)
tune.out <- tune(svm, Purchase ~ ., data = train, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
#evaluate 
ypred <- predict(bestmod, test)
table(predict = ypred, truth = test$Purchase)
#test MSE
t<-table(predict = ypred, truth = test$Purchase)
(t[1,2]+t[2,1])/200
#-------------c----------------
set.seed(1)
tune.poly<- tune(svm, Purchase ~ ., data = train, kernel = "polynomial", degree=2, ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.poly)

bestmod <- tune.poly$best.model
summary(bestmod)
#evaluate 
ypred <- predict(bestmod, test)
table(predict = ypred, truth = test$Purchase)
#test MSE
t<-table(predict = ypred, truth = test$Purchase)
(t[1,2]+t[2,1])/200
#--------------d---------------
set.seed(1)
tune.out <- tune(svm, Purchase ~ ., data = train,  kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
#evaluate 
ypred <- predict(bestmod, test)
table(predict = ypred, truth = test$Purchase)
#test MSE
t<-table(predict = ypred, truth = test$Purchase)
(t[1,2]+t[2,1])/200
