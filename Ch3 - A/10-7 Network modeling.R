library(igraph)
library(graph)

#RNGkind("Mersenne-Twister")	# randomly-generated graph
#set.seed(123)
#g1 <- randomGraph(letters[1:10],1:4,p=0.3)	# link to other node with probability 0.3

# generate random graph with x nodes and p probability of being connected to each other node
g <- erdos.renyi.game(10,1/3)
plot(g)
l <-layout.fruchterman.reingold(g)	#adjust layout
plot.igraph(g, layout=l)

# eigenvalye centrality calculation, get centrality vector
res <- evcent(g)
res$vector	#these are the weights of the nodal importance

# find connected components
clusters(g)

# Use random walk trap to detect communities
wtc <- walktrap.community(g)
res <- community.to.membership(g, wtc$merges, step=3)
print(res)

# compute the modularity
#print(modularity(g, res$membership, weights=E(g)$weight))



# find shortest paths
shortest.paths(g)
get.shortest.paths(g,0)




######## create adjacency matrix A and create graph
A <- matrix(nrow=4,c(0,1,1,1, 1,0,1,0, 1,1,0,1, 1,0,1,0))
g3 <- graph.adjacency(A)
evcent(g3)


A <- matrix(nrow =3,c(0,1,1, 1,0,0, 1,0,0))



# generate edge list with connected nodes and edge distance
#el <- matrix(nc = 3, byrow=TRUE, c(0,1,8, 0,3,4, 1,3,3, 3,1,1, 1,2,1, 1,4,7, 3,4,4, 2,4,1, 0,5,2))
#g2 <- add.edges(graph.empty(5), t(el[,1:2]), weight=el[,3])

#plot(g2)
