# This code is programmed in R v2.15.2.with iGraph v0.7.0 and netrw v0.2.6


# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")
library("netrw")

####################  QUESTION 1 ####################

graph_data = read.table("sorted_directed_net.txt", sep = "\t", header = FALSE) # read text file
colnames(graph_data) = c("Node 1", "Node 2", "weights")
g1 = graph.data.frame(graph_data,directed = TRUE) # covert table to directed garph

connectivity = is.connected(g1) #check if network is connected

if (connectivity == 1) {
  sprintf('The network is connected')
} else {
  sprintf('The newtork is not connected')
}
