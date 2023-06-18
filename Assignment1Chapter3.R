# Chapter 3 - Problem 8
library(ISLR)
Auto = read.csv("https://www.statlearning.com/s/Auto.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "?")
Auto = na.omit(Auto)
lm.fit <- lm(mpg ~ horsepower, data=Auto)
summary(lm.fit)

predict(lm.fit, data.frame("horsepower"=98), interval="confidence")

predict(lm.fit, data.frame("horsepower"=98), interval="prediction")

plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
abline(lm.fit, lwd=3, col="red")

par(mfrow=c(2,2))
plot(lm.fit)
# Chapter3 - Problem 9
head(Auto)

Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
head(Auto)

pairs(Auto)

cor(Auto[, !(names(Auto)=="name")])

cor(Auto[,-c(8, 9)])

lm.fit <- lm(mpg ~ .-name, data=Auto)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)


lm.fit.inter = lm(mpg ~ (.-name)*(.-name), data=Auto)
summary(lm.fit.inter)

apply(Auto,2,range)
lm.fit.1 <- lm(I(mpg^2) ~ cylinders + I(displacement^2) + I(horsepower^2) + sqrt(weight) + I(acceleration^2) + sqrt(year) + origin, data=Auto)
summary(lm.fit.1)
