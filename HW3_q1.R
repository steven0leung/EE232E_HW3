# This code is programmed in R v2.15.2.with iGraph v0.7.0 and netrw v0.2.6


# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")
library("netrw")

####################  QUESTION 1 ####################

graph_data <- read.table("sorted_directed_net.txt", sep = "\t", header = FALSE) # read text file
colnames(graph_data) <- c("Node 1", "Node 2", "weights")
g1 <- graph.data.frame(graph_data,directed = TRUE) # covert table to directed garph

connectivity <- is.connected(g1,mode = "strong") #check if network is connected

if (connectivity == 1) {
  sprintf('The network is connected')
} else {
  sprintf('The newtork is not connected')
  cl <- clusters(g1, mode = "strong")
  gccIndex <- which.max(cl$csize)
  nonGCCnodes <- (1:vcount(g1))[cl$membership != gccIndex]
  gcc <- delete.vertices(g1,nonGCCnodes)
}

####################  QUESTION 2 ####################

g1_indeg <- degree(gcc , mode = "in")
g1_outdeg <- degree(gcc , mode = "out")

hist(g1_indeg, breaks <- seq(from = min(g1_indeg), to = max(g1_indeg), by=1),main = "GCC In-Degree Distribution",xlab = "In-Degree",ylab = "Frequency")
hist(g1_outdeg, breaks <- seq(from = min(g1_outdeg), to = max(g1_outdeg), by=1),main = "GCC Out-Degree Distribution",xlab = "Out-Degree",ylab = "Frequency")

####################  QUESTION 3 ####################

# option 1 : keep the number of edges unchanged, and just remove the directions
  # using label.propagation.community
undirected_1 <- as.undirected(gcc, mode = "each")
community_1 <- label.propagation.community(undirected_1, weights = E(undirected_1)$weights)

m_1 <- modularity(community_1)
s_1 <- sizes(community_1)

# option 2 : merge the two directed edges between i and j
edge_weights <- function(weight) sqrt(prod(weight))
undirected_2 <- as.undirected(gcc, mode = "collapse", edge.attr.comb = edge_weights)

  # using label.propagation.community
community_2a <- label.propagation.community(undirected_2, weights = E(undirected_2)$weights)
m_2a <- modularity(community_2a)
s_2a <- sizes(community_2a)

  # using fastgreedy.community
community_2b <- fastgreedy.community(undirected_2, weights = E(undirected_2)$weights)
m_2b <- modularity(community_2b)
s_2b <- sizes(community_2b)

####################  QUESTION 4 ####################

# Sub GCC
max_com_index <- which.max(s_2b)
non_sub_GCC_nodes <- (1:vcount(undirected_2))[community_2b$membership != max_com_index]
sub_GCC <- delete.vertices(undirected_2, non_sub_GCC_nodes)

# Sub GCC Community
sub_GCC_com <- fastgreedy.community(sub_GCC, weights=E(sub_GCC)$weights)
sub_GCC_m <- modularity(sub_GCC_com)
sub_GCC_s <- sizes(sub_GCC_com)

####################  QUESTION 5 ####################

large_community_index = which(sizes(community_2b) > 100)

for (i in 1:length(large_community_index)) {
  nodes_to_del <- (1:vcount(undirected_2))[community_2b$membership != large_community_index[i]]
  temp_sub_graph <- delete.vertices(undirected_2, nodes_to_del)
  temp_sub_com <- fastgreedy.community(temp_sub_graph)
  # Print results
  cat('\n Sub-Community number', i , 'whose size greater than 100 \n')
  cat('Modularity: ',modularity(temp_sub_com),'\n')
  print(sizes(temp_sub_com))
  cat('\n')
}


