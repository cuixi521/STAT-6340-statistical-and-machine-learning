#=============Q1==================#
planet<-read.table("C:/Users/xicui/Desktop/stat6340/planet.csv", header = TRUE, sep=",")
#---------a exploratory---------
attach(planet)
str(planet)
summary(planet)
plot(planet,col = c("green","blue"))
par(mfrow = c(1,2))
#histofram
par(mfrow = c(1,3))
hist(planet$Mass)
hist(planet$Period)
hist(planet$Eccentricity)
par(mfrow = c(1,3))
plot(density(planet$Mass))
plot(density(planet$Period))
plot(density(planet$Eccentricity))
#bivariate comparing 
par(mfrow =c(1,3))
plot(Mass,Period)
abline(reg = lm(Period ~ Mass), col = "red")
plot(Mass,Eccentricity)
abline(reg = lm(Eccentricity ~ Mass), col = "red")
plot(Eccentricity, Period)
abline(reg = lm(Period ~ Eccentricity), col = "red")
#correlation between two variables
cor(planet)
#-------------d------------
install.packages("mclust")
#standardizing the dataset
data<-scale(planet)
#hierarchically cluster
hc.complete=hclust(dist(data),method="complete")
par(mfrow =c(1,1))
plot(hc.complete)
rect.hclust(hc.complete,k=3,border = c("blue","red","green"))
groups <-cutree(hc.complete, 3)
#pairwise scatterplots
library(mclust)
hc<-clPairs(planet,groups,symbol=16, main="Pairwise Scatterplots")
clPairsLegend('topright', class = hc$class, col = hc$col, pch = hc$pch,cex=0.8,horiz=TRUE, title = "Clusters")
#summarize cluster-specific means of three variables
aggregate(planet, by=list(cluster=groups),mean)
#-----------e-K means with K=3 cluster----------
km.out <- kmeans(planet, 3, nstart = 1)
km.out
km.out$cluster
#summarize cluster-specific means of three variables
aggregate(planet, by=list(km.out$cluster),mean)
#pairwise scatterplots
km<-clPairs(planet,km.out$cluster,symbol=16, main="Pairwise Scatterplots")
clPairsLegend('topright', class = km$class, col = km$col, pch = km$pch,
              cex=0.8,horiz=TRUE, title = "Clusters")

table(groups,km.out$cluster)
#===============Q2=======================#
track<-read.table("C:/Users/xicui/Desktop/stat6340/track-records-women.csv", header = TRUE, sep=",")
#---------a exploratory-------------------------------------
head(track)
str(track)
library(car)
scatterplotMatrix(track)
cor(track[,2:8])
#----------c standardzing and PCA----------------------------
track.standar <- as.data.frame(scale(track[2:8]))
track.pca<-prcomp(track.standar)
summary(track.pca)
screeplot(track.pca, type="lines", main="Scaled PCA")
#------------d--------------------
plot(track.pca$x[,1],track.pca$x[,2])
text(track.pca$x[,1],track.pca$x[,2], track$country, cex=0.7, pos=4, col="red")
#table of components
track.pca$rotation[,1]
track.pca$rotation[,2]
track.pca$rotation[,1:2]
biplot(track.pca, scale=0)
#-------------------e Rank the nations based on their score-----------
library(plyr)
score=data.frame(track.pca$x[,1])
rank=cbind(track$country,score)
arrange(rank,desc(score))
#===============Q3=======================#
library(ISLR)
#--------a-------------
#data preparing
attach(OJ)
data<-OJ
train<-data[1:870,]
test<-data[871:1070,]
#--------b----------
#fit the tree model
library(tree)
tree<- tree(Purchase ~ ., train)
tree
summary(tree)
#plot the tree
plot(tree)
text(tree, pretty = 0, cex = 0.9)
#---------------c  pruning ---------------
#predict class for tesing data
tree.pred <- predict(tree, test, type = "class")
#Compute the confusion matrix
table(tree.pred, test$Purchase)
#Compute the test misclassification rate
(11+38)/200
#Perform cost complexity pruning by CV, guided by misclassification rate
set.seed(3)
cv.oj <- cv.tree(tree, FUN = prune.misclass)
cv.oj

#find best tree size
par(mfrow = c(1, 1))
plot(cv.oj$size, cv.oj$dev, type = "b")
plot(cv.oj$k, cv.oj$dev, type = "b")
cv.oj$size[which.min(cv.oj$dev)]#best size is 7 no need to prune

#try prune of size = 4
prune.oj <- prune.misclass(tree, best = 4)
plot(prune.oj)
text(prune.oj, pretty = 0)
#Compute the test misclassification rate
tree.predict <- predict(prune.oj, test, type = "class")
table(tree.predict, test$Purchase)
#----------d Bagging approach--------
library(randomForest)
set.seed(1)
bag <- randomForest(Purchase ~ ., train, mtry=17, ntree = 1000, importance = TRUE)
bag

#estimate the test error rate
bag.pred<-predict(bag,test,type="class")
table(bag.pred, test$Purchase)
(19+17)/200
#Get variable importance measure for each predictor
?importance
importance(bag)
varImpPlot(bag)
#-----------e Randomforest approach-------------
set.seed(1)
rf <- randomForest(Purchase ~ ., train, mtry=sqrt(17),ntree = 1000, importance = TRUE)
rf
#estimate the test error rate
rf.pred<-predict(rf,test,type="class")
table(rf.pred, test$Purchase)
(16+25)/200
#Get variable importance measure for each predictor
importance(rf)
varImpPlot(rf)

#---------------f Boosting approach-------------
library(gbm)
train$Purchase<-ifelse(train$Purchase=="CH","1","0")
test$Purchase<-ifelse(test$Purchase=="CH","1","0")

set.seed(1)
#boosting in classifying
boost<- gbm(Purchase ~ ., train, distribution = "bernoulli", n.trees = 1000, interaction.depth = 1,shrinkage = 0.01)
summary(boost)
#estimate the test error rate
predict(boost,test,n.trees = 1000,type = "response")
boost.prob = predict(boost,test, n.trees = 1000, type = "response")
#depending on probability define the class
boost.pre<-ifelse(boost.prob>0.5,"1","0")
#confution matrix
t<-table(boost.pre,test$Purchase)
t
#test MSE
(t[1,2]+t[2,1])/200

#----------g knn approach--------------
library(class)
data$Store7<-ifelse(data$Store7=="Yes",1,0)
train<-data[1:870,]
test<-data[871:1070,]
#prepare training and testing data to predictors and class label
train.X<-train[,c(2:18)]
train.Y<-train$Purchase
test.X<-test[,c(2:18)]
test.Y<-test$Purchase
#fit knn and find the optimal K
ks <- c(seq(1, 30, by = 1), seq(35, 100, by = 5))
nks <- length(ks)
err.rate.train <- numeric(length = nks)
err.rate.test <- numeric(length = nks)
names(err.rate.train) <- names(err.rate.test) <- ks

for (i in seq(along = ks)) {
  set.seed(1)
  mod.train <- knn(train.X, train.X, train.Y, k = ks[i])
  set.seed(1)
  mod.test <- knn(train.X, test.X, train.Y, k = ks[i])
  err.rate.train[i] <- 1 - sum(mod.train == train.Y)/length(train.Y)
  err.rate.test[i] <- 1 - sum(mod.test == test.Y)/length(test.Y)
}
result <- data.frame(ks, err.rate.train, err.rate.test)
result[err.rate.test == min(result$err.rate.test), ]
#optimal K=7
set.seed(1)
knn.fit <- knn(train.X, test.X, train.Y, k =7 , prob = F)
#test MSE
t<-table(knn.fit,test.Y)
t
(t[1,2]+t[2,1])/200


