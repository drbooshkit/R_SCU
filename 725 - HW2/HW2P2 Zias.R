#QUESTION 2 (Find relationships)
#Go to the web page for Federal Reserve data at:http://research.stlouisfed.org/
#Download exchange rate data for 6 countries of your choice versus the US dollar. Summarize this data and plot the series. Provide a brief description.
library("quantmod")
library("tseries")
library("car")
library("stats")

date_range <- "2000-01-01::2013-10-15"
# Canada, China, India, Japan  to $1 US
getSymbols(c("DEXCAUS", "DEXCHUS", "DEXINUS", "DEXJPUS", "DEXUSEU", "DEXUSUK"), src='FRED')

# convert xts from getSymbols to numeric
canadaUS <- as.numeric(DEXCAUS[date_range])
chinaUS <- as.numeric(DEXCHUS[date_range])
indiaUS <- as.numeric(DEXINUS[date_range])
japanUS <- as.numeric(DEXJPUS[date_range])
USEU <- as.numeric(DEXUSEU[date_range])
USUK <- as.numeric(DEXUSUK[date_range])
EUUS <- 1/USEU
UKUS <- 1/EUUS

# create data frame
fxrates <- data.frame(canadaUS, chinaUS, indiaUS, japanUS, EUUS, UKUS)
fxrates <- na.omit(fxrates)

par(mfrow=c(3,2))
plot(fxrates$canadaUS, ylab="Candadian Dollar per $1 US", type="l")
plot(fxrates$chinaUS, ylab="China per $1 US", type="l")
plot(fxrates$indiaUS, ylab="India Dollar per $1 US", type="l")
plot(fxrates$japanUS, ylab="Japan Yen per $1 US", type="l")
plot(fxrates$EUUS, ylab="Euro per $1 US", type="l")
plot(fxrates$UKUS, ylab="UK per $1 US", type="l")


#Present the correlation table of exchange rates.
cor(fxrates)

#Present the correlation table of changes in exchange rates.
# create matrix of changes (percent changes)
n <- dim(fxrates)[1]
fxrates.change <- (fxrates[2:n,]-fxrates[1:(n-1),])/fxrates[1:(n-1),]
cor(fxrates.change)

#Pick your favorite pair of exchange rates and say whether the correlation of exchange rate changes in statistically significant or not. How would you establish this?
#Regress one series of exchange rate changes on another, and describe the output (R-square, t-statistics, f-statistic, etc.) Is there any economic conclusion that you can infer from the regression?
res <- lm(fxrates.change$canadaUS ~ fxrates.change$EUUS)
summary(res)

# Using the data series you have, can you build a model that uses lagged values of changes in exchange rates to predict future exchange rate changes with a high level of accuracy? (This requires some experimentation and playing with the data. Ideally program R to run all possible cases.)
# this is one rate against all the others, current rate
n <- length(fxrates.change$canadaUS)
res <- lm(fxrates.change$canadaUS ~ fxrates.change$chinaUS + fxrates.change$indiaUS + fxrates.change$japanUS + 
	fxrates.change$EUUS + fxrates.change$UKUS)
summary(res)

# this is one rate against all the others, ;agged once
res <- lm(fxrates.change$canadaUS[2:n] ~ fxrates.change$chinaUS[1:(n-1)] + fxrates.change$indiaUS[1:(n-1)] + fxrates.change$japanUS[1:(n-1)] + 
	fxrates.change$EUUS[1:(n-1)] + fxrates.change$UKUS[1:(n-1)])
summary(res)



# Use a DurbinWatson to test for auto-correlation of Canada:US exchange rate
res <- lm(fxrates.change[2:n,1] ~ fxrates.change[1:(n-1),1])
summary(res)
durbin.watson(res,max.lag=10)


#Run a vector autoregression (VAR) on the data of exchange rate changes that you have. What inferences can you make from the results?
var6 = ar(fxrates.change,aic=TRUE,order=6)
var6$order
var6$ar
