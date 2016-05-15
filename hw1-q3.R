#####question 3 #####
#install.packages("ElemStatLearn")
rm(list=ls())
library(ElemStatLearn)
library(MASS)
data(Boston)
head(Boston)
dim(Boston)
apply(Boston, 2, summary)

boston <- Boston

par(mfrow=c(3,5))
for(i in 1:ncol(Boston)){
        truehist(Boston[,i], main=colnames(Boston)[i], col="gray", border="white")
        d <- density(Boston[,i])
        lines(d, col="red")
}

drops <- c("rad", "zn", "chas", "black")
boston <- boston[,!(names(boston) %in% drops)]


hist(log(Boston$crim))


#discretize columns - could have used discretize()
boston$indus <- ordered(cut(boston$indus, c(0,15,30), labels=c("low", "high")))
boston$nox <- ordered(cut(boston$nox, c(0.3, 0.5, 0.7, 0.9), labels=c("low", "moderate", "high")))
boston$crim <- ordered(cut(boston$crim, c(0,2,20,90), labels=c("Safe", "Moderate", "Dangerous")))
boston$rm <- ordered(cut(boston$rm, c(3.5, 6.2, 9), labels=c("3 to 6", "6 or more")))
boston$ptratio <- ordered(cut(boston$ptratio, c(12.6,17.4,20.2,23), labels=c("Below Average", "Average", "Above Average")))
boston$age <- ordered(cut(boston$age, c(0, 25, 50, 75, 100),
                          labels=c("0-25 years", "25-50 years", "50-75 years", "50-100 years")))
boston$dis <- ordered(cut(boston$dis, c(1,2,3,4,5,50),
                      labels=c("1-2", "2-3", "3-4", "4-5", ">5")))
boston$tax <- ordered(cut(boston$tax, c(180, 350, 715),
                          labels=c("Low", "High")))
boston$lstat <- ordered(cut(boston$lstat, c(1, 5, 10, 15, 40), 
                        labels=c("Low", "Medium", "High", "Very high")))
boston$medv <- ordered(cut(boston$medv, c(10, 20, 30, 40), 
                           labels=c("Low", "Middle", "High")))

#Convert to a binary incidence matrix
library(arules)
Bos <- as(boston, "transactions")
summary(Bos)

itemFrequencyPlot(Bos, support = 0.01, cex.names = 0.8)

# Apply the apriori algorithm
rules  <- apriori(Bos, parameter = list(support = 0.05, confidence = 0.6))

#take a closer look at differences
summary(rules)
names(boston)
?apriori
as(head(Bos,5), "matrix")

rulesLowCrimeDisLow <- subset(rules, subset = rhs %in% "crim=Safe" & lift>1)
inspect(head(sort(rulesLowCrimeDisLow, by = "support"), n = 100))

rulesLowPTRatio <- subset(rules, subset = rhs %in% "ptratio=Below Average" & lift > 1)
inspect(head(sort(rulesLowPTRatio, by = "support"), n = 10))
head(Boston)

