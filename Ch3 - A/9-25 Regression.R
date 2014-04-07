#must load package in memory:
library(quantmod)

#list what's in memory
#ls()

date_range = "2007-01-01::2011-03-31"

getSymbols(c("GOOG","AAPL","CSCO","IBM"))
goog = GOOG[date_range]
aapl = AAPL[date_range]
csco = CSCO[date_range]
ibm = IBM[date_range]

##quick stock charts, see quantmod site for examples
#chartSeries(SPXU)

#get last column, closing price
goog = as.numeric(GOOG[,6])
aapl = as.numeric(AAPL[,6])
csco = as.numeric(CSCO[,6])
ibm = as.numeric(IBM[,6])

#combine vectors into columns of table
stks = cbind(goog,aapl,csco)

#compute returns between one day and the next, continuously compounding
#we care about returns, not prices typically
n <- dim(stks)[1]
rets = log(stks[2:n,]/stks[1:(n-1),])

#lm is linear model, least squares.  LHS ~ RHS1+RHS2...
res = lm(rets[,1] ~ rets[,2] + rets[,3])

#r-squared, % of this result is described by these variables
#F-statistic, probability that RHS variables explain LHS. do these variables make a good model?
summary(res)

#verify linear regression via matrix manipulation:
Y = matrix(rets[,1],1694,1)
X = cbind(matrix(1,1694,1), rets[,2:3])

#solve is inverse of matrix
beta = solve(t(X) %*% X) %*% (t(X) %*% Y)