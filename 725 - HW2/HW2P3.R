#QUESTION 2 (Find relationships)
#Go to the web page for Federal Reserve data at:http://research.stlouisfed.org/
#Download exchange rate data for 6 countries of your choice versus the US dollar. Summarize this data and plot the series. Provide a brief description.
library("quantmod")
library("tseries")
library("car")
library("stats")


date_range <- "2000-01-01::2013-10-15"
# get symbols
getSymbols(c("AAPL","^GSPC"))

# convert xts from getSymbols to numeric
aapl <- as.numeric(AAPL[date_range])
gspc <- as.numeric(GSPC[date_range])

# create data frame
stocks <- data.frame(aapl,gspc)
stocks <- na.omit(stocks)

#Present the correlation table of changes in exchange rates.
# create matrix of changes (percent changes)
n <- dim(stocks)[1]
rets <- (stocks[2:n,]-stocks[1:(n-1),])/stocks[1:(n-1),]
logRets <- log(stocks[2:n,]/stocks[1:(n-1),])

# linear regression of returns
res <- lm(logRets$aapl ~ logRets$gspc)
summary(res)


# Using the data series you have, can you build a model that uses lagged values of changes in exchange rates to predict future exchange rate changes with a high level of accuracy? (This requires some experimentation and playing with the data. Ideally program R to run all possible cases.)

res <- lm(logRets$aapl[5:n] ~ logRets$aapl[4:(n-1)] + logRets$aapl[3:(n-2)] + logRets$aapl[2:(n-3)] + logRets$aapl[1:(n-4)])
summary(res)

res <- lm(logRets$gspc[5:n] ~ logRets$gspc[4:(n-1)] + logRets$gspc[3:(n-2)] + logRets$gspc[2:(n-3)] + logRets$gspc[1:(n-4)])
summary(res)

# Use a DurbinWatson to test for auto-correlation
dwres <- durbin.watson(logRets$aapl,max.lag=10)


n <- length(logRets$aapl)
res <- lm(logRets$aapl[2:n] ~ logRets$aapl[1:(n-1)])
durbin.watson(res,max.lag=10)

