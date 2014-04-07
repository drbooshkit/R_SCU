library(rpart)
data(kyphosis)
head(kyphosis)

# create a "class"ification tree (vs. regression tree)
fit = rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
summary(fit)

# plot the tree with splits
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=0.8)

