library("fpp")
library("tseries")
library("sde")
library("gdata")
library("reshape")
library("base")

# From the FRED web site (http://research.stlouisfed.org/fred2/)
# download the 3-month LIBOR rate for as long a period as possible. Use daily data. 
data=read.csv(file="e:/dropbox/fnce 696/data/3MonthLIBOR.csv",header=TRUE)

# convert character date to Date type
#PRICE$DATE <- as.Date(as.character(PRICE$DATE),format="%Y%m%d")

# plot the data and add in moving averages
plot(data$libor, main="3-Month LIBOR Rate", ylab="LIBOR Rate, %", xlab="Day")


#fit <- lm(data$libor ~ data$date)


ma100 <- ma(data$libor, order=101)
ma50 <- ma(data$libor, order=51)

lines(ma100, col="red")
lines(ma50, col="green")

# check for mean-reverting hypothesis
# adf.test(data$libor)

x <- data$libor
n <- length(x)
daily_rets = log(x[2:n]/x[1:(n-1)])
plot(daily_rets)

daily_rets_plus1 <- daily_rets[2:n]

# this creates two columns, r_0, r_1
test <- data.frame(cbind(daily_rets_plus1[1:(n-2)], daily_rets[1:(n-2)]))
test <- rename(test,c(X1="r_0", X2="r_1"))





#test <- data.frame(data[1:nrow(test),1:2], test)		#create a data frame that has a date, price, and returns

#test$date <- as.Date(as.character(test$date),format="%Y-%m-%d"))
#test$date <- as.character(test$date)

#dates <- as.Date(data[,1], "%m/%d/%Y")
#data <- transform(data,
#Date <- as.Date(test$Date))


#http://www.sitmo.com/article/calibrating-the-ornstein-uhlenbeck-model/

# mean reversion with a 'reversion speed'  b
# For commodities and interest rates (and perhaps for exchange rates) mean-reversion model has more economic logic than the geometric Brownian model presented before. 
# The most basic mean-reversion model is the (arithmetic) Ornstein-Uhlenbeck model
# In other words the log stock value is pulled back a fraction b of how far it had deviated in the previous period from its expected value.


#### Z CODE, OLD
#delta <- .250
#lambda <- -1*log(a)/delta
#mu <- b/(1-a)
#sigma <- sd(fit$residuals)*sqrt((-2*log(a))/(delta*(1-a**2)))
# simulation equation
#normal <- dnorm(1000, 0, 1)
#s_0 <- test$r_0[length(test$r_0)]
#h <- 50
#fcast <- s_0
#for(i in 1:h){
# e_1 <- s_0 * exp(-delta*lambda) + mu*(1-exp(-lambda*delta)) + sigma*sqrt((1-exp(-2*lambda*delta))/(2*lambda))*sample(normal,1)
#  s_0 <- e_1
#  fcast <- rbind(fcast,e_1)
#}

#forecast <- c(test$r_0, fcast)
#test.xts  <- as.xts(test)

# convert to time series and test
#testts <- ts(test)
#adf.test(testts)



### AFTER CLASS walk-through

# model
#rt1 <- A + B*rt + e_t
# regression will minimize e_t

# if you performed a least squares linear regression in Excel of historical rt+1 against rt:
plot(test$r_1 ~ test$r_0)
fit <- lm(test$r_1 ~ test$r_0, test)	#fit is linear model of r_1 to r_0






a <- fit$coefficients[1]	#slope
b <- fit$coefficients[2]	#intercept
sigma2 <- sd(fit$residuals)






# send initial guess
params <- c(a, b, sigma2)

# with MLE, we say we have more information, "the distribution of error is normal":
# assume e_t is normal. e_t ~ N(0,sigma^2)
# choose initial A, B, sigma^2. then generate e given A,B,r0, r1. 
LL = function(params, test) {
 
  A <- params[1]
  B <- params[2]
  sigma2 <- params[3]
	data2 <- test
 n <- nrow(data2)
# compute column of e_t
  e <- data2$r_1 - A - B*data2$r_0	# as vectors

# compute column of f(e)
  fe <- exp(-.5*e^2/sigma2^2)/sqrt(2*pi*sigma2^2)
  LL <- -sum(log(fe))
}

#e_MLE <- LL(params, test)

e_MLE <- nlm(LL, params, test)

# this should yield higher quality A and B by assuming/forcing a distribution assumuption.
# trying different distributions could yield better fits


#(a) Using an ordinary least squaresregression fit a mean-reverting model to this interest rate series. What are the coefficients of the model? 

#(b) Is there a trend #to the rate? If so, how might you modify your model to account for this trend as well? 
#lam <- BoxCox.lambda(x)

## Forecast fucntion.Use selected model to forecast out h horizons
# fcast <- forecast(x,h=500)
# plot(fcast,lwd=2




#(c) How might you estimate your model using maximum-likelihood methods instead of a linear regression? Explain and implement.