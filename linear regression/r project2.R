#Data preparing
data<-read.table("C:/Users/xicui/Desktop/crime.csv", header = TRUE, sep=",")

#summery of mean statistics
summary(data)
str(data)
data$region<-as.factor(data$region)
str(data)
#Full modle multile linear regression
fit.full<-lm(murder.rate~poverty+high.school+college+single.parent+unemployed+metropolitan+region,data=data)
summary.lm(fit.full)

# Residual plot
plot(fitted(fit.full), resid(fit.full))
abline(h = 0)
# QQ plot
qqnorm(resid(fit.full))
# Time series plot of residuals
plot(resid(fit.full), type="l")
abline(h=0)

#Final model
library(MASS)
step <- stepAIC(fit.full, direction="both")
step$anova

# Residual plot
plot(fitted(step), resid(step))
abline(h = 0)
# QQ plot
qqnorm(resid(step))
# Time series plot of residuals
plot(resid(step), type="l")
abline(h=0)

#Q2.predict the murder.rate
predict(step, newdata=data.frame(high.school=mean(data$high.school),single.parent=mean(data$single.parent),metropolitan = mean(data$metropolitan),region ='South'))




