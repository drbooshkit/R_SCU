
#data(cu.summary)
#head(cu.summary)
# explain mileage given other attributes
#fit <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)


library(tree)
cahomes = read.table("e:/datamining/datasets/cahomedata.txt", header=TRUE)

fit <- tree(log(MedianHouseValue)~Longitude+Latitude,data=cahomes)
plot(fit)
text(fit,cex=0.8)

