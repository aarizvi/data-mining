#######################################################
########## STA 546 HW #2
########## Created by: Abbas Rizvi
########## Due: March 10th 23:59
#######################################################


############# QUESTION 1 #################

rm(list=ls())
setwd("/Users/aarizvi/Google Drive/STA546/hw2/")
list.files()
load("SwissBankNotes.rdata")

head(SwissBankNotes)
SwissBankNotes <- cbind(SwissBankNotes, as.factor(c(rep("genuine",100), rep("counterfeit", 100))))
colnames(SwissBankNotes)[7] <- "class"

gen.swiss <- prcomp(SwissBankNotes[SwissBankNotes$class == 'genuine',-7], center=TRUE, scale=FALSE) #first 100 are genuine
cf.swiss <- prcomp(SwissBankNotes[SwissBankNotes$class == 'counterfeit',-7], center=TRUE, scale=FALSE) #next 100 are counterfeit
all.swiss <- prcomp(SwissBankNotes[, -7], scale=FALSE)




#install.packages("devtools")
library(devtools)
#install_github("ggbiplot", "vqv")

library(ggbiplot)
gbiplots <- function(x){
        g <- ggbiplot(x, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
        print(g)
}
screeplot(gen.swiss, type="lines")
gbiplots(gen.swiss)
screeplot(cf.swiss, type="lines")
gbiplots(cf.swiss)
screeplot(all.swiss, type="lines")

gbiplots.m <- function(x, groups){
        g <- ggbiplot(x, obs.scale = 1, var.scale = 1, ellipse = TRUE,
                      groups = groups, circle = TRUE)
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
        print(g)
}

gbiplots.m(all.swiss, SwissBankNotes$class)

############# QUESTION 2 #################
rm(list=ls())

## part a
#generate simulated data with 20 observations 
SimDat <- function(mean, sd, group){
        n <- 20 #20 observations
        m <- 50 #50 variables
        x <- matrix(rnorm(n*m,mean=mean,sd=sd),n,m) 
        dimnames(x) <- list(rownames(x, do.NULL=FALSE, prefix=paste0("Obs-",deparse(substitute(group)),sep="-")),
                            colnames(x, do.NULL=FALSE, prefix="Var")) #differentiating the observations names
        return(x)
}

set.seed(333)
rmat <- as.matrix(rbind(SimDat(0,2, class1), SimDat(5,8, class2), SimDat(10,8, class3)))

## part b
groups = c(rep("class 1", 20), rep("class 2", 20), rep("class 3", 20))
rmat <- data.frame(cbind(rmat,groups))
rmat.pc <- prcomp(rmat, center=TRUE, scale=FALSE)

#plot the two prinicpal component vectors 
gbiplots.m(rmat.pc, groups)

## part c

#apply k-means
km.k3<- kmeans(rmat[,-51], centers=3) #perform k-means clustering with K=3
table(km.k3$cluster, groups)

#can calculate FPR and TPR



## part d
km.k2<- kmeans(rmat[,-51], centers=2) #perform k-means clustering with K=2
table(km.k2$cluster, groups)


km.k4 <- kmeans(rmat[,-51], centers=4)
table(km.k4$cluster, groups)

## part e - perform K-means clustering with K=3 on the first two PC score vectors
km.pc <- kmeans(as.matrix(rmat.pc$x[,1:2]), centers=3)
table(km.pc$cluster, groups)


## part f -- use scale() function -- perform K-means clustering with K=3 on the data after scaling\
#each variable to hvae standard deviation one. How does these results compare to part b?

#standardize <- function(x) {(x - mean(x))/sd(x)} #center and scaling
#scale.mat <- apply(rmat, 2, standardize)

rmat.sc <- scale(rmat, center = FALSE, scale = apply(rmat, 2, sd))

km.sc <- kmeans(rmat.sc, centers=3)
table(km.sc$cluster, groups)



######### QUESTION 3 ############
rm(list=ls())
list.files()
dat <- read.csv("Ch10Ex11.csv", header=F)
head(dat)


colnames(dat)[1:20] <- paste0("healthy-", 1:20)
colnames(dat)[21:40] <- paste0("diseased-", 1:20)


## apply hierarchical clustering to the samples -- part b

dissimilarity <- 1-cor(dat)
dat.dist <- as.dist(dissimilarity)
plot(hclust(dat.dist, method="complete"), main="Dissimilarity = 1 - Correlation (Complete Linkage)", xlab="")
plot(hclust(dat.dist, method="average"), main="Dissimilarity = 1 - Correlation (Average Linkage)", xlab="")
plot(hclust(dat.dist, method="single"), main="Dissimilarity = 1 - Correlation (Single Linkage)", xlab="")


# collaborator wants to know which genes are most different across the treatment groups
# simple t-test to check differences across genes ...

myttest <- function(x) t.test(x[1:20], x[21:40])$p.value
p.val <- apply(dat, 1, myttest)
sorted_p <- order(p.val)
dat.order <- dat[sorted_p, ]
DEgenes <- dat.order[1:100,]
DEgenes[1:5,1:5]


## lets try a multT test

labels <- factor(c(rep("healthy", 20), rep("diseased", 20)))
mt.test <- mt.maxT(dat, classlabel=labels, B=10000)
sorted_mt <- dat[mt.test$index,]
sorted_mt[1:5,1:5]
