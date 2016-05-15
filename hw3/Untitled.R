##########################
####### HOMEWORK 3 #######
##########################
rm(list=ls())



## Using the hierarchical functions in 'igraph' perform the following tasks:

# Part a
# find a consensus dendrogram that is based on MCMC-based sampling and produce a plot
# that reveals communities
# hrg consensus features do not work as they should
# if you want to produce optimal communities -- try 'optimal.community' but this is done via a fast greedy method, not hrg
# therefore it is not necessry for the homework

library(igraph)
set.seed(1)
#Undirected social network of frequent associations between 62 dolphins in a community living off Doubtful Sound, New Zealand.
dolphins <- nexus.get("dolphins") 

#Coappearance weighted network of characters in the novel Les Miserables.
#Two characters are connected if they appear in the same scene. The weight of the connection is the number of common appearances.
miserables <- nexus.get("miserables") 

#markov chain ... random process that undergoes transitions from one state to another on a state space.
#must possess a property "memorylessness"
#memorylessness -- probability distribution of the new state depends only on the current state and not on the sequence of events that preceded it.

#Metropolis algorithm -- MCMC

#goal: sample from p, or approximate E(f(x)) (x~p)

#This function calculates the optimal community structure of a graph, by maximizing the modularity measure over all possible partitions.
optcom <- optimal.community(dolphins) #communities is an igraph object
V(dolphins)$comm <- membership(optcom) #membership gives vertices 
par(mfrow=c(2,2), mar=c(0, 0, 3, 0))
plot(optcom, dolphins)
hrg <- fit_hrg(dolphins) #MCMC sampling
hrg
print(hrg, level=5)
#plot full hierarchy as an igraph graph
ihrg <- as.igraph(hrg)
ihrg$layout <- layout.reingold.tilford(ihrg)
plot(ihrg, vertex.size=10, edge.arrow.size=0.2)
#customize the plot a bit, show probabilities and communities
vn <- V(ihrg)$name
colbar <- rainbow(length(optcom))
vc <- ifelse(is.na(V(ihrg)$prob), colbar[V(dolphins)$comm], "darkblue")
V(ihrg)$label <- ifelse(is.na(V(ihrg)$prob), vn, round(V(ihrg)$prob, 2))
plot(ihrg, vertex.size=10, edge.arrow.size=0.2, vertex.shape="none", vertex.label.color=vc,
     main="Hierarchical network model of the dolphins")
library(ape)
# plot it as dendrogram, looks better if the 'ape package is installed
plot_dendrogram(hrg)


#create consesus dendrogram from munltiple samples, takes long....
hcons <- consensus_tree(hrg)





?walktrap.community #random walk .... 
?edge.betweenness.community
?fastgreedy.community
?optimal.community
?spinglass.community
comm.dendrograms <- function(network){
        x <- network
        wc <- walktrap.community(network)
        sampling <- cut(as.dendrogram(wc), h=length(membership(wc))-length(wc))$lower
        plot.new()
        layout(matrix(1:6, nrow=2))
        dendPlot(wc, mode="hclust")
        lapply(sampling, plot, cex = 1)
}

comm.dendrograms(dolphins)
comm.dendrograms(miserables)


# part b







