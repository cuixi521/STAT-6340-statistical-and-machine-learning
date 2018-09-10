train<-read.table("C:/Users/xicui/Desktop/1-training_data.csv", header = TRUE, sep=",")
test<-read.table("C:/Users/xicui/Desktop/1-test_data.csv", header = TRUE, sep=",")
train.X <- train[,c(1:2)]
train.Y<-train$y
test.X<-test[,c(1:2)]
test.Y<-test$y

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

plot(ks, err.rate.train, xlab = "Number of nearest neighbors", ylab = "Error rate", 
     type = "b", ylim = range(c(err.rate.train, err.rate.test)), col = "blue", pch = 20)
lines(ks, err.rate.test, type="b", col="purple", pch = 20)
legend("bottomright", lty = 1, col = c("blue", "purple"), legend = c("training", "test"))

result <- data.frame(ks, err.rate.train, err.rate.test)

result[err.rate.test == min(result$err.rate.test), ]

n.grid <- 7
x1.grid <- seq(f = min(train.X[, 1]), t = max(train.X[, 1]), l = n.grid)
x2.grid <- seq(f = min(train.X[, 2]), t = max(train.X[, 2]), l = n.grid)
grid <- expand.grid(x1.grid, x2.grid)

k.opt <- 7
set.seed(1)
mod.opt <- knn(train.X, grid, train.Y, k = k.opt, prob = T)
prob <- attr(mod.opt, "prob") # prob is voting fraction for winning class
prob <- ifelse(mod.opt == "yes", prob, 1 - prob) # now it is voting fraction for Y == "yes"
prob <- matrix(prob, n.grid, n.grid)

plot(train.X, col = ifelse(train.Y == "yes", "green", "red"))

contour(x1.grid, x2.grid, prob, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)

