source("get rets.R")

date_range <- "2007-01-01::2011-03-31"
symbol <- "GOOG"

	rets <- get_rets(symbol,date_range)
	

#create table of correlation
#cor(rets)

#covariance matrix
#cov(rets)

#mean of daily returns
#mean(rets[,1])

#annualize returns
#mean(rets[,1])*252

#means of each column, annualized
#colMeans(rets)

#standard deviation per day GOOG
#sd(rets[,1])

#stddev per annum GOOG
#sd(rets[,1])*sqrt(252)

#skewness(rets[,1])
#kurtosis(rets[,1])

#hist(rets[,1], 50, col=3, lwd=2, lty=2, main="GOOG", xlab="Days", ylab="Price, $")
