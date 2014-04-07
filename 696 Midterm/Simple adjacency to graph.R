library(igraph)
a <- matrix(nc=4, byrow=TRUE, c(0,1,1,0, 1,0,1,0, 1,1,0,1, 1,0,1,0))
a

g <- graph.adjacency(a, mode="undirected", weighted=TRUE)
res <- evcent(g)

res$vector

res <- evcent(g, scale=FALSE)
res$vector
plot(g)


## betweeness

el <- matrix(nc =3, byrow=TRUE,
	c(0,1,0, 0,2,2, 0,3,1, 1,2,0, 1,4,5, 1,5,2, 2,1,1, 2,3,1,
	  2,6,1, 3,2,0, 3,6,2, 4,5,2, 4,7,8, 5,2,2, 5,6,1, 5,8,1,
	  5,9,3, 7,5,1, 7,8,1, 8,9,4) )

g <- add.edges(graph.empty(10), t(el[,1:2]), weight=el[,3])
res <- betweenness(g)
res