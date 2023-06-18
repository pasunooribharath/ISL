#Chapter 2 - problem 8
install.packages("ISLR")
college <- read.csv("https://www.statlearning.com/s/College.csv", header = TRUE, stringsAsFactors = TRUE)
head(college)


rownames(college) <- college[, 1]
head(college)
View(college)

college <- college[, -1]
head(college)
View(college)


summary(college)
pairs(college[, 1:10])

plot(college$Private, college$Outstate, xlab = "Private", ylab = "Outstate")


str(college$Private)

Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

summary(college)
summary(college$Elite)

plot(college$Elite, college$Outstate, xlab="Elite", ylab="Outstate")



par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of Student applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "Yes"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "No"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "Yes"], xlab = "Number of applicants", main = "Histogram for elite schools")


par(mfrow=c(2,2))

hist(college$Top10perc, xlab="Top10perc", breaks=10, col="red", main="Percentage of The Top10 H.S. Students")
hist(college$Apps, breaks=10, xlab="Apps", col="orange", main="Number of New Applications Received")
hist(college$Personal, xlab="Personal", breaks=10, col="green", main="Estimated Personal Spending")
hist(college$PhD, xlab="PhD", breaks=10, col="blue", main="Percentage of Faculty with Ph.D.'s")


par(mfrow=c(2,2))
hist(college$Books, col = 2, breaks = 50, xlab = "Books", ylab = "Count")
hist(college$PhD, col = 3, breaks = 50, xlab = "PhD", ylab = "Count")
hist(college$Grad.Rate, col = 4, breaks = 50, xlab = "Grad Rate", ylab = "Count")
hist(college$perc.alumni, col = 6, breaks = 50, xlab = "% alumni who donate", ylab = "Count")


summary(college$PhD)
#College with more than 100% of percentage
row.names(college[college$PhD>100, ])


par(mfrow = c(2, 2))
hist(college$perc.alumni, xlab = "Percent of alumni who donate", main = "Histogram for all colleges")
hist(college$perc.alumni[college$Private == "Yes"], xlab = "Percent of alumni who donate", main = "Histogram for private schools")
hist(college$perc.alumni[college$Private == "No"], xlab = "Percent of alumni who donate", main = "Histogram for public schools")
hist(college$perc.alumni[college$Elite == "Yes"], xlab = "Percent of alumni who donate", main = "Histogram for elite schools")


# Exploring the relationship between Grad.Rate(Graduation Rate) and S.F.Ratio(Student/faculty ratio).
plot(college$S.F.Ratio, college$Grad.Rate, xlab = "Student to Faculty Ratio", ylab = "Graduation Rate", main = "Plot of Grad.Rate vs S/F Ratio")


# Linear regression line.
abline(lm(college$Grad.Rate~college$S.F.Ratio), col="red")








#Chapter 2 - Problem 9

Auto = read.csv("https://www.statlearning.com/s/Auto.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)

data(Auto)
str(Auto)
glimpse(Auto)
head(Auto)
summary(Auto)


sapply(Auto[,1:7], range)

sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)

Auto.reduced = Auto[-c(10:84),]
sapply(Auto.reduced[,1:7], range)
sapply(Auto.reduced[,1:7], mean)
sapply(Auto.reduced[,1:7], sd)
pairs(Auto[1:8])


par(mfrow=c(2,2))
plot(Auto$displacement, Auto$acceleration)
plot(Auto$weight, Auto$horsepower)
plot(Auto$cylinders, Auto$mpg)
plot(Auto$weight, Auto$mpg)

#cor function to find the correlation.
cor(Auto$weight, Auto$horsepower) 

cor(Auto$displacement, Auto$horsepower) 

cor(Auto$displacement, Auto$weight) 


str(Auto$year)
Auto$year<-as.factor(Auto$year)
plot(Auto$year, Auto$mpg, varwidth=T, xlab = "Year", ylab ="Mileage", col = "blue", pch=19)
plot(Auto$acceleration, Auto$mpg, xlab = "Acceleration", ylab ="Mileage", col = "black", pch=19)

cor(Auto$acceleration, Auto$mpg)




#test
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg, xlab = "Engine displacement (cubic inches)", ylab = "Miles per gallon")
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "Miles per gallon")
plot(Auto$year, Auto$mpg, xlab = "Model Year", ylab = "Miles per gallon")

par(mfrow = c(2, 2))
plot(Auto$year, Auto$acceleration, xlab = "Model Year", ylab = "0 to 60mph time (seconds)")
plot(Auto$year, Auto$displacement, xlab = "Model Year", ylab = "Engine displacement (cubic inches)")
plot(Auto$year, Auto$weight, xlab = "Model Year", ylab = "Car weight (pounds)")
plot(Auto$year, Auto$horsepower, xlab = "Model Year", ylab = "Horsepower")

par(mfrow = c(2, 2))
plot(Auto$weight, Auto$acceleration, xlab = "Car weight (pounds)", ylab = "0 to 60mph time (seconds)")
plot(Auto$cylinders, Auto$acceleration, xlab = "Number of engine cylinders", ylab = "0 to 60mph time (seconds)")
plot(Auto$displacement, Auto$acceleration, xlab = "Engine displacement (cubic inches)", ylab = "0 to 60mph time (seconds)")
plot(Auto$horsepower, Auto$acceleration, xlab = "Horsepower", ylab = "0 to 60mph time (seconds)")


par(mfrow = c(2, 1))
plot(Auto$weight, Auto$horsepower, xlab = "Car weight (pounds)", ylab = "Horsepower")
plot(Auto$weight, Auto$displacement, xlab = "Car weight (pounds)", ylab = "Engine displacement (cubic inches)")

str(Auto$origin)
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")

# Chapter 2 - problem 10

library(MASS) 
Boston
?Boston
head(Boston)
dim(Boston)


pairs(Boston)



Boston.corr = cor(Boston)
Boston.corr.crim = Boston.corr[-1,1]
print(Boston.corr.crim[order(abs(Boston.corr.crim), decreasing = T)])

par(mfrow=c(2,2))
# get the four most correlated variables
aux = names(Boston.corr.crim[order(abs(Boston.corr.crim), decreasing = T)][1:4])
aux
for(i in aux){
  plot(Boston$i, Boston$crim, xlab=i, ylab="crime")
}

par(mfrow = c(2, 2))
hist(Boston$crim, xlab = "Per capita crime rate", main = "Histogram of Boston crime rates")
hist(Boston$tax, xlab = "Tax rate per 10000 USD", main = "Histogram of Boston tax rates")
hist(Boston$ptratio, xlab = "Pupil-teacher ratio", main = "Histogram of Boston pupil-teacher ratios")

summary(Boston$crim)
summary(Boston$tax)

sum(Boston$chas)
table(Boston$chas)

median(Boston$ptratio)
summary(Boston$ptratio)


subs.lw = which(Boston$medv<median(Boston$medv))
print(subs.lw)

Boston.corr.subs.lw = cor(Boston[subs.lw, ])
corr.compare = data.frame('lower'=Boston.corr.subs.lw[, "medv"], 'all'=Boston.corr[, "medv"])
corr.compare$diff = corr.compare$lower - corr.compare$all
hist(corr.compare$diff, xlab="Correlation Differences")

hist(abs(corr.compare$diff), xlab="Correlation Differences")

main.diffs = head(corr.compare[order(abs(corr.compare$diff), decreasing = T), ], 5)

print(main.diffs)

print(rownames(main.diffs))

hist(Boston$rm, main="Distribution of Rooms by Dwelling", xlab="Rooms")

length(Boston$rm[Boston$rm>7])
sum(Boston$rm > 7)


length(Boston$rm[Boston$rm>8])
sum(Boston$rm > 8)

frm =as.factor(as.character(lapply(Boston$rm, function(x) ifelse(x>8, "]8, +Inf[", ifelse(x>7,"]7,8]","[0,7]")))))
plot(frm, Boston$medv, varwidth=T, xlab="Number of Rooms", 
     ylab="Median Values by $1000s")

Boston[Boston$rm>8 & Boston$medv<30, ]

