
# The script generates ROC points by taking different amounts of best predicted edges from the predicted edge set.

plotRoc <- function(originalGraph, sampledGraph, minConnectionsToAdd, maxConnectionsToAdd, step, mode)
{
	options(error=recover)


	actual_edgeList <- get.edgelist(originalGraph)
	nodeCount <- max(actual_edgeList) + 1
	potentialConnections <- nodeCount * (nodeCount - 1) / 2


	source("simList.R")
	source("predictLinks.R")
	source("roc.R")



	if (mode == 1)
	{
		# The Jaccard similarity coefficient of two vertices is the number of common neighbors divided by the number of vertices that are 
		# neighbors of at least one of the two vertices being considered. similarity.jaccard calculates the pairwise Jaccard similarities for some 
		# (or all) of the vertices.  

		SJ <- similarity.jaccard(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"), loops = FALSE)
		sim <- SJ
	}
	else if (mode == 2)
	{
		# The Dice similarity coefficient of two vertices is twice the number of common neighbors divided by the sum of the degrees of the 
		# vertices. similarity.dice calculates the pairwise Dice similarities for some (or all) of the vertices.
	
		SD <- similarity.dice(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"), loops = FALSE)
		sim <- SD
	}
	else if (mode == 3)
	{
		# The inverse log-weighted similarity of two vertices is the number of their common neighbors, weighted by the inverse logarithm of their degrees. 
		# It is based on the assumption that two vertices should be considered more similar if they share a low-degree common neighbor, since high-
		# degree common neighbors are more likely to appear even by pure chance. Isolated vertices will have zero similarity to any other vertex. Self-
		# similarities are not calculated.

		SInLogW <- similarity.invlogweighted(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"))
		sim <- SInLogW
	}
	else
		stop("unknown mode specified")

	list <- simList(sim)



	treshold <- 0



	maxi <- min(maxConnectionsToAdd, potentialConnections, nrow(list))
	numOutRows <- (maxi - minConnectionsToAdd) / step + 1
	rocPoints <- mat.or.vec(numOutRows, 2)

	i <- minConnectionsToAdd
	i_out <- 1


	while (i_out <= numOutRows)
	{
		numBestToTake <- i


		predictedGraph <- predictLinks(sampledGraph, list, numBestToTake, treshold)
		rocr <- roc(originalGraph, sampledGraph, predictedGraph)


		rocPoints[i_out, 1] = rocr[2]		#fpr as x
		rocPoints[i_out, 2] = rocr[1]		#tpr as y

		i <- i + step
		i_out <- i_out + 1

	} 	# for (i in 1:potentialConnections)


	rocPoints
}
