
# The script calculates the position (true positive rate and false positive rate) of one Roc point 
# given an actual graph, sampled graph and predicted graph that is created by using data in sampled graph

roc <- function(actualGraph, 
	sampledGraph, 
	predictedGraph)
{
	actual_edgeList <- get.edgelist(actualGraph)
	sampled_edgeList <- get.edgelist(sampledGraph)
	predicted_edgeList <- get.edgelist(predictedGraph)

	falsePositives <- 0
	truePositives <- 0
	falseNegatives <- 0
	# trueNegatives <- 0

	actual_rows <- nrow(actual_edgeList)
	for (i in 1:actual_rows)
	{
		node1 <- actual_edgeList[i, 1]
		node2 <- actual_edgeList[i, 2]

		if (!are.connected(predictedGraph, node1, node2))
			falseNegatives <- falseNegatives + 1
	
		
	}

	predicted_rows <- nrow(predicted_edgeList)
	for (i in 1:predicted_rows)
	{
		node1 <- predicted_edgeList[i, 1]
		node2 <- predicted_edgeList[i, 2]

		if (!are.connected(actualGraph, node1, node2))
			falsePositives <- falsePositives + 1

		else if (!are.connected(sampledGraph, node1, node2))
			truePositives <- truePositives + 1
	}

	
	nodeCount <- max(actual_edgeList) + 1
	potentialConnections <- nodeCount * (nodeCount - 1) / 2
	negatives <- potentialConnections - actual_rows 	# number of nonconnected node pairs (connections not to be predicted)

	positives <- actual_rows - nrow(sampled_edgeList) 	# number of node pairs to predict


	trueNegatives <- negatives - falsePositives;

	if (positives > 0)
		tpr <- truePositives / positives
	else
		tpr <- 0
		
	if (negatives > 0)
		fpr <- falsePositives / negatives
	else
		fpr <- 0


	c(tpr, fpr, falseNegatives, truePositives, falsePositives, trueNegatives, positives, negatives, potentialConnections)
	# c(tpr, fpr)

}



