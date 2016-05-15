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

library(RBGL)
library(ggm)
library(gRbase)
library(gRain)
library(Rgraphviz)

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
dolphins.optcom <- optcom
save(file="dolphins_optcom.Rdata", dolphins.optcom)
load("dolphins_optcom.Rdata")
V(dolphins)$comm <- membership(dolphins.optcom) #membership gives vertices 
plot(dolphins.optcom, dolphins)
hrg <- hrg.fit(dolphins) #MCMC sampling
#fit_hrg fits a HRG to a given graph. 
#It takes the specified steps number of MCMC steps to perform the fitting, or a convergence criteria if the specified number of steps is zero
hrg
print(hrg, level=5)
#plot full hierarchy as an igraph graph
ihrg <- as.igraph(hrg)
ihrg$layout <- layout.reingold.tilford(ihrg)
plot(ihrg, vertex.size=10, edge.arrow.size=0.2)
#customize the plot a bit, show probabilities and communities
vn <- V(ihrg)$name
colbar <- rainbow(length(dolphins.optcom))
vc <- ifelse(is.na(V(ihrg)$prob), colbar[V(dolphins)$comm], "darkblue")
V(ihrg)$label <- ifelse(is.na(V(ihrg)$prob), vn, round(V(ihrg)$prob, 2))
par(mar=c(0, 0, 3, 0))
plot(ihrg, vertex.size=10, edge.arrow.size=0.2, vertex.shape="none", vertex.label.color=vc,
     main="Hierarchical network model of the dolphins")
library(ape)
# plot it as dendrogram, looks better if the 'ape package is installed
plot_dendrogram(hrg)

####### PART B ########
## create noisy dataset ... delete 5% of edges randomly (track which ones they are)
# perform MCMC on this data followed by link-prediction
# are you able to predict edges that you deleted at random well?

link.prediction <- function(igraph.object, cutoff.percentage){
        #set seed before sampling
        set.seed(1)
        #sample edges
        sample.edges <- sample(1:length(E(igraph.object)),
                               round(length(E(igraph.object))*(cutoff.percentage/100)))
        igraph.subset <- delete.edges(igraph.object, sample.edges)
        pred <- hrg.predict(igraph.subset)
        pred.edges <- pred$edges
        pred.prob <- pred$prob
        deleted.edges.idx <- get.edges(igraph.object, sample.edges)
        rowmatch <- function(A,B) { 
                f <- function(...) paste(..., sep=":") 
                if(!is.matrix(B)) B <- matrix(B, 1, length(B)) 
                a <- do.call("f", as.data.frame(A)) 
                b <- do.call("f", as.data.frame(B)) 
                match(b, a) 
        } 
        correct.pred.idx <- rowmatch(pred.edges, deleted.edges.idx)
        num.correct <- length(correct.pred.idx)
        total.predictions <- nrow(pred.edges) 
        pred.deleted.edges.prob <- pred$prob[correct.pred.idx]
        vertices <- V(igraph.object)[pred.edges[correct.pred.idx,]]$name
        answer <- cbind(vertices[1:(length(vertices)/2)],
                        vertices[((length(vertices)/2)+1):length(vertices)],
                        round(pred.deleted.edges.prob,4))
        colnames(answer) <- c("v1", "v2", "predicted probability")
        answer
}
set.seed(1)
link.prediction(dolphins, 5)

#part c 
link.prediction(dolphins, 15)
link.prediction(dolphins, 40)


### question 2

# one operation on BN that arise sin many settings is the marginalization of some node in the network
# let the orginal BN be denoted as B
# construct a BN B' over all the nodes EXCEPT for Alarm that is the minimal I-map for the:
# marginal distn P_{B}(B,E,T,N,J,M)

#minimal I-map - graph K is a minimal I-map for a set of independencies I if the removal of a single edge K renders its not an Imap

#to construct a minimal Imap for the marginalized network
#we preserve all indepdencies that exist in G when the variable being marginal A is 


#minimal I-map may fail to capture a lot of struture
#even if present and representable as a bayesian graphical model (PGM)

b <- DAG(alarm~burglary, alarm~earthquake, john~alarm, john~tv,  mary~alarm, mary~nap, order=FALSE)
b.graph <- as(b, "graphNEL")
b.prime <- inducedDAG(b, order=c("burglary", "earthquake", "tv", "nap", "john", "mary"))
b.prime.graph <- as(b.prime,"graphNEL")

plot(b.graph, main = "Original Bayesian Network B")
plot(b.prime.graph, main="Minimal I-map for Marginal Bayesian Network B'")

## question 3

dag <- list(~a, ~c|a, ~d|a:b, ~b, ~e|b, ~g|d:e, ~f|c:e, ~g|d:e, ~h|f:g)

dag.list <- dagList(dag)
dSep(as(dag.list, "matrix"), "c", "g", NULL)
dSep(as(dag.list, "matrix"), "c", "e", NULL)
dSep(as(dag.list, "matrix"), "c", "e", "g")
dSep(as(dag.list, "matrix"), "a", "g", c("d","e"))
dSep(as(dag.list, "matrix"), "a", "g", "d")

dCon <- function(dSep){
        if(dSep==FALSE){
                as.logical(dSep+1)
        }
        else{
                as.logical(dSep*0)
        }
}
?dSep

