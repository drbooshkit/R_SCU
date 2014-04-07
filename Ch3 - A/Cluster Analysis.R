ncaa = read.table("e:/dropbox/fnce 696/data/ncaa.txt", header=TRUE)

# split 64 teams into 4 clusters
fit <- kmeans(ncaa[,4:14],4)
fit
fit$size

# how to plot 11-dimension space? just pick 2 which have the biggest variation in the means
attribs <- c(4,6)
plot(ncaa[,attribs],col=fit$cluster)
points(fit$centers[attribs],col=1:4,pch=6,cex=3)


##### RANDOM example, different distribution means = good clustering for example
# generate 64x64 Euclidean distance matrix
d <- dist(ncaa[,4:14])

library(graphics)

# 50 x 2 matrix of random numbers X 2
x <- rbind(matrix(rnorm(100,sd=0.3),ncol=2),matrix(rnorm(100,mean=1,sd=0.3),ncol=2))

# assume these are random characteristics of a population
# create them into clusters
colnames(x) <- c("X","Y")

k <- 20
# use k-means of k clusters
cl <- kmeans(x,k)

# this can tell you which attribute is better (more differentiating):
# Cluster means:
#           X           Y
#1 1.00869805 0.981538988
#2 0.02806985 0.001552783

# plot and color by cluster
plot(x, col=cl$cluster)
# add centroids to plot
points(cl$centers,col=1:k,pch=6,cex=3)

res <- predict(cl, c(12,12))