rm(list=ls())
setwd("/Users/aarizvi/Google Drive/STA546/hw1/")
load("bodyfat.rdata")
View(bodyfat)


#first change everything that is in imperical measurements to metric measurements
bodyfat$weight <- round((bodyfat$weight / 2.2)*1000, 2) #convert weight from lbs into grams
bodyfat$height <- round(bodyfat$height * 2.54, 2) #convert height from inches into cm

#visualize boxplots for all columns
par(mfrow=c(3,5))
colnames <- dimnames(bodyfat)[[2]]
for(i in 1:ncol(bodyfat)){
        boxplot(bodyfat[,i], main=colnames[i])
}

#visualize histograms for all columns
library(MASS)
par(mfrow=c(3,5))
for(i in 1:ncol(bodyfat)){
        truehist(bodyfat[,i], main=colnames[i], col="gray", border="white")
             d <- density(bodyfat[,i])
             lines(d, col="red")
}

#now its time to remove outliers
indices <- list()
for(i in 1:ncol(bodyfat)){
        indices[[i]] <- which(!bodyfat[,i] %in% boxplot.stats(bodyfat[,i])$out == 'FALSE')      
}
outlier.indices <- unique(unlist(indices))
bodyfat.trim <- bodyfat[-c(outlier.indices),]

#take a look at histograms/boxplots again
par(mfrow=c(3,5))
for(i in 1:ncol(bodyfat.trim)){
        truehist(bodyfat.trim[,i], main=colnames[i], col="gray", border="white")
        d <- density(bodyfat.trim[,i])
        lines(d, col="red")
}

