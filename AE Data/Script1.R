AEdata <- read.csv(file="e:/datamining/ae data/quota per FTE.csv", header=TRUE)


results <- lm(AEdata[,3] ~ AEdata[,2])
