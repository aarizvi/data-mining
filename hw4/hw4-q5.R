###############################
### Homework 4 - Question 5 ###
######### Abbas Rizvi #########
###############################
rm(list=ls())
setwd("/Users/aarizvi/Google Drive/STA546/hw4/")

## Data released from the US Department of Commerece, Beureau of the Census is available in R
data(state)
?state
## First need to pre-process data
head(state.x77)
state.x77 <- data.frame(state.x77)
rownames(state.x77) <- state.abb
state.x77[["Population"]] <- state.x77[["Population"]]/1000 #Population in millions
#per capita income measures average income earned per person in a given area
#calculated by dividing area total income by its total population
state.x77[["Income"]] <- state.x77[["Income"]]*10 #Per Capita Income in Tens of Thousands
pairs(state.x77)

##################
##### PART A #####
##################
## Focus on the data {Population, Income, Illiteracy, Life Expectancy, Murder, HS Grad, Frost, Area}
## Cluster this data using at least two tools we learned in STA 546
## E.g. k-means, hierarchical clustering, SOM, PCA
## Keep the class labels (region or state name) in mind, but do not use them in the modeling.
## Report your detailed findings

## Hierarchical clustering
library(dendextend)
hclust.state <- function(data, distance.metric, linkage.method, group){
        dissimilarity <- 1-cor(t(data))
        dis <- dist(dissimilarity, method=distance.metric)
        hc <- hclust(dis, method=linkage.method)
        hcd <- as.dendrogram(hc)
        if(length(group)<=5){
                colpal <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
                labels_colors(hcd) <- colpal[group][order.dendrogram(hcd)] # Assigning the labels of dendrogram object with new colors:
                hcd2 <- hcd %>% set("labels_cex", c(1,1))
        }
        else{
                colpal <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')
                labels_colors(hcd) <- colpal[group][order.dendrogram(hcd)]
                hcd2 <- hcd %>% set("labels_cex", c(1,1))
        }
        plot(hcd2, main=paste(distance.metric,' distance/',linkage.method, ' linkage', sep=""), cex.main=1)
        legend("topright", text.col=colpal, cex=1, levels(group))
}

linkage <- c("single", "complete", "average")
distance <- c("manhattan", "euclidean")

par(mfrow = c(3,1), mar = c(2,2,2,2))
for(i in 1:length(linkage)){
        for(j in 1:length(distance)){
                hclust.state(state.x77, distance[j], linkage[i], state.region)
                hclust.state(state.x77, distance[j], linkage[i], state.division) 
        }
}

hclust.cat <- function(data, distance.metric, linkage.method){
        dissimilarity <- 1-cor(data)
        dis <- dist(dissimilarity, method=distance.metric)
        hc <- hclust(dis, method=linkage.method)  
        plot(hc, main=paste(distance.metric, ' distance/',linkage.method, ' linkage', sep=""), xlab=NULL)
}
par(mfrow=c(3,1))
for(i in 1:length(linkage)){
        for(j in 1:length(distance)){
                hclust.cat(state.x77, distance[j], linkage[i])
        }
}

## PCA 
fit <- prcomp(state.x77, center=TRUE, scale=TRUE)
summary(fit)
screeplot(fit, type="lines")

## lets look at a biplot too and color the dots by state abbreviations
library(ggbiplot)
mybiplot <- function(groups=region, group.title="Legend"){
g <- ggbiplot(fit, obs.scale=2, var.scale=1, groups=groups, labels = state.abb, labels.size=6, varname.size=7, varname.adjust=1.2)
g <- g + theme(legend.text=element_text(size=14), legend.key.size=unit(1, "cm"), legend.title=element_text(size=14, face="bold" ),
               axis.title=element_text(size=14, face="bold"))
g <- g + labs(color=group.title)
g
}     

mybiplot(state.regions, "State Regions")
mybiplot(state.division, "State Divisions")



##################
##### PART B #####
##################
## Build a Gaussian Graphical Model using Graphical Lasso for the 8 predictors mentioned in Part A.
## What do you find for different penalities, and how does it complement (and/or contradict) your results in part A?
library(gRbase)
library(gRim)
library(gRain)
library(glasso)

# Look at partial correlation
S.body <- cov.wt(state.x77, method = "ML")
PC.body <- cov2pcor(S.body$cov)
heatmap(PC.body) 

S <- S.body$cov 

# Estimate a single graph
m0.lasso <- glasso(S, rho = 0.1) 
names(m0.lasso)
#we are interested in 'wi'
#'wi' is the estimated inverse covariance matrix
my.edges <- m0.lasso$wi != 0 #if edge its TRUE if theres not an edge its FALSE
diag(my.edges) <- FALSE #edges pointed to own vertices need to be 0. we don't want to see these self-loops
g.lasso <- as(my.edges, "graphNEL") # convert for plotting, adjacency matrix
nodes(g.lasso) <- names(state.x77) #label graphNEL object with column names
glasso.net <- cmod(g.lasso, data = state.x77) #cmod ... specify GGM, c stands for continuous
plot(glasso.net)


# Estimate over a range of rho's
rhos <- c(2, 4, 6, 10, 15, 30) #model selection aspect ... try range of rhos (complexity parameter?)
#rho -- vector of non-negative regularization parameters for the lasso
#should be increasing from smallest to largest value of rho
m0.lasso <- glassopath(S, rho = rhos)
par(mfrow=c(3,2))
for (i in 1:length(rhos)){
        my.edges <- m0.lasso$wi[, , i] != 0 #stack of matrices
        diag(my.edges) <- FALSE
        g.lasso <- as(my.edges, "graphNEL") # convert for plotting
        nodes(g.lasso) <- names(state.x77)
        glasso.net <- cmod(g.lasso, data = state.x77)
        plot(glasso.net)
}
