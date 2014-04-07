
##call system OS
#system(os command)

#read in CSV file:
data = read.csv("e:/dropbox/fnce 696/data/aapl.csv", header=TRUE)

#give me all rows and column Y
stkp = data[,1]
stkp

##plot vector with line type options
#plot(stkp, type="l", col=3, lwd=2, lty=2, main="AAPL", xlab="Days", ylab="Price, $")

#must load package in memory:
library(quantmod)

#list what's in memory
#ls()

date_range = "2007-01-01::2013-10-15"

getSymbols(c("GOOG","AAPL","CSCO","IBM"))
goog = GOOG[date_range]
aapl = AAPL[date_range]
csco = CSCO[date_range]
ibm = IBM[date_range]

##quick stock charts, see quantmod site for examples
chartSeries(SPXU)

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
plot(rets[,1])

#create table of correlation
cor(rets)

#covariance matrix
cov(rets)

#mean of daily returns
mean(rets[,1])

#annualize returns
mean(rets[,1])*252

#means of each column, annualized
colMeans(rets)

#standard deviation per day GOOG
sd(rets[,1])

#stddev per annum GOOG
sd(rets[,1])*sqrt(252)

skewness(rets[,1])
kurtosis(rets[,1])

hist(rets[,1], 50, col=3, lwd=2, lty=2, main="GOOG", xlab="Days", ylab="Price, $")

#call maximizer to find best mean and sd

#call external file
#source("NormLL.R")
LL = function(p,x) {
	alpha = p[1]; sigsq = p[2]
	logf = -log(sqrt(2*pi*sigsq))
				- (x-alpha)^2/(2*sigsq)
LL = -sum(logf)
}

p = c(0.001,.001)
x = rets[,1]

LL(p,x)
#call maximizer tool
res = nlm(LL,p,x)

#get back estimate of two parameters mu and sig^2
#res$estimate, get just piece of list with attribute $xxx
sig2 = res$estimate[2]/(1/252)
sig = sqrt(sig2)

library(car)
durbin.watson(rets,max.lag=10)