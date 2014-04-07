
ncaa = read.table("e:/dropbox/fnce 696/data/ncaa.txt", header=TRUE)

d <- dist(ncaa[,4:14])

fit <- hclust(d,method="ward")
names(fit)
plot(fit)

rect.hclust(fit, k=4,border="red")

# what is the order of observations along the bottom of the tree?
fit$order

# in which group is each observation
groups <- cutree(fit, 4)
groups

# create cluster plot
# this will pick two most important components
library(cluster)
clusplot(ncaa[,3:14],groups,color=TRUE)