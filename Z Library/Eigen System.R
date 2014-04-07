tryrates <- read.table(file="e:/datamining/datasets/tryrates.txt", header=TRUE)

rates <- tryrates[,2:9]

rates <- as.matrix(rates)


cor(rates)


eigen(cov(rates))

