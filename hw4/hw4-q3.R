###############################
### Homework 4 - Question 3 ###
######### Abbas Rizvi #########
###############################
rm(list=ls())
setwd("/Users/aarizvi/Google Drive/STA546/hw4/")
library(glasso)

## Consider the following webgraphs.

##### PART A #####
## Compute the PageRank vector of Webgraph A for damping constants p = 0.05, 0.25, 0.50, 0.75, and 0.95.
## How sensitive is the PageRank vector, and overall ranking of importance, to the damping constant.
## Does the relative ranking of importance according to PageRank support your intuition?

# Construct an Igraph
nodes <- data.frame(names=c("A","B","C","D","E","F"))
relations <- data.frame(
        from = c("C","F","B","B","E","D","D"),
        to = c("A","C","C","E","D","B","E")
)
g <- graph.data.frame(relations, directed = TRUE, vertices = nodes)
dev.off()
plot(g, main="Webgraph A")
pg <- page.rank(g)
pg$vector

p <- c(0.05,0.25,0.51,0.75,0.95) # a typical damping factor is 0.15
pg <- list()
for(i in 1:length(p)){
        name <- paste('p=',p[i],sep='')
        pg[[name]] <- page.rank(g, damping=p[i])
}

sort(pg$`p=0.05`$vector, decreasing=TRUE)
sort(pg$`p=0.25`$vector, decreasing=TRUE)
sort(pg$`p=0.5`$vector, decreasing=TRUE)
sort(pg$`p=0.75`$vector, decreasing=TRUE)
sort(pg$`p=0.95`$vector, decreasing=TRUE)

##### PART B #####
## Compute the PageRank vector of Webgraph B for damping constant p = 0.15
## Intepret your results in terms of the relationship between number of incoming links that each node has.
## Does the relative ranking of importance according to PageRank support your intuition?
nodesB <- data.frame(names=c("A","B","C","D","E","F","G","H"))
relationsB <- data.frame(
        from = c("B","C","D","E","F","G","H"),
        to = c("A","A","B","B","C","C","C")
)
gb <- graph.data.frame(relationsB, directed = TRUE, vertices = nodesB)
dev.off()
plot(gb, main="Webgraph B")

pgr <- page.rank(gb, damping=0.15) 
sum(pgr$vector) #sums to 1
sort(pgr$vector, decreasing=TRUE)
pgr

#adj <- as(gb, "matrix")
#id <- matrix(0, nrow=8,ncol=8)
#diag(id) <- 1
#B = matrix(1,nrow=8,ncol=8)
#M = (1-0.15)*adj+0.15*B
