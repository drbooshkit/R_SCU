# working with time series
#https://www.otexts.org/fpp/resources
#http://cran.r-project.org/web/views/TimeSeries.html
#

#Bring in Hyndman's library from Forecasting: principles and practice
require(fpp)  #includes forecast package

data = read.csv("e:/dropbox/fnce 696/data/balance.csv", header=TRUE)
#bitcoin = read.csv("e:/dropbox/fnce 696/data/bitcoin_dollar.csv", header=TRUE)

#x <- aggregate(bitcoin[,2])  #bring in example fpp data #Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form. 
x <- data[,1]
fit <- x #skip all data transform


#split-data into train and test data
#x_train <- window(x,end="1999.99")
#x_test <- windows(x,start="2000")


#Simple forcast methods: USE AS BENCHMARKS to test advanced methods
#1. Mean Method - forecast h future values as MEAN of observations
#meanf <- meanf(x,h=20)	#h is number of steps ahead
#2. Naive method - forecast equal to LAST observed value
#naive <- naive(x,h=20)
#rwf <- rwf(x,h=20)	#random walk model
#3. Seasonal Naive - forecast equal to LAST value from same SEASON
#snaive <- snaive(x,h=20)
#4. Drift method - forecast euals to LAST value plus AVG CHANGE over period.  equiavalent to line drawn between first and last observations
#rwfd <- rwf(x,drift=TRUE,h=20)

#More forecast models
#croston()	#supply forecasting
#stlf()	#STL decompoition of time series
#holt()	#hold method
#ses <- ses(x)	#single exponential series
#splinef()	#splinebased forecast
#thetaf()	#included but not used

#these return 'forecast class': original series, point forecasts, prediction interval, method used, residuals
#use $ to access lists. using forecast() on any data set will convert it to forecast class
#result_class <- forecast(x)
#attributes(result_class) #see what's inside it

#Automated Modeling
#plot(forecast(x))	#AUTOMATIC MODEL. picks up trend, shows 80% and 95% interval.

#check for ACCURACY
#MAE - mean absolute error
#MSE - mean squared error
#MAPE - absolute percentage error. use %error. normalizes, but only good if y >> 0 (y has natural zero)
#MASE - take absolute differences and scale with factor, q
#accuracy(meanf)	#In-sample accuracry ONLY, not good for forecasting
#accuracy(rwfd)	#In-sample accuracry ONLY, not good for forecasting




#Exponential smoothing - originated in Business forecasting
#ses()
#Trend and Seasonal components combined in various forms:
#(N,N) - simple exponential smoothing
#(A,N) - Holt's linear method
#(A,A) - additive Holt-Winters method
#(A,M) - multiplicative Holts-Winters
#(Ad,M)- - damped multiplicative H-W
#HoltWinters() #implements the above methods minimizine MSE
##BEST: ETS Model
## Auto ETS model Hyndman implimentation uses all using likelihood estimation
##over 30 variations of ETS models (add or multiply error? eg)
##returns object of CLASS ets. stick that into forecast.
##check out model terms. SIGMA is std dev of error.
##AIC is measure of out-of-model fit.  the ETS function minimizes AIC
#fit <- ets(x) #error, trend, seasonal)

##Box-Cox transformation
##scaled transformations to stabilize variance
##change shape using LAMBDA factor scaling
##create equal-amplitude seasonal variation graphically
#lam <- BoxCox.lambda(x)
#fit <- ets(x, addititive=TRUE, lambda=lam)

###Seasonal and non-seasonal data decomposition with ARIMA
#fit <- auto.arima(x, lambda=lam)


##Forecast fucntion
#Use selected model to forecast out h horizons
fcast <- forecast(fit,h=50)	
plot(fcast,lwd=4)

##test accuracy of fit
#test <- ets(x_test,model=fit)	#uses OLD model 'fit' to test new data
#accuracy(test)
#accuracy(fcast(fit,30), x_test))


###High frequency seasonality. ETS
#taylor <- stl(taylor,s,window=7)
#plot(taylor)
#must have MANY periods
#decomp <- stl(x)
#fcast <- stlf(x)
#plot(decomp)
#plot(fcast)

#multiple seasons
#dshw()  #allows two seasonal periods, such as weekly/annual, or overlapping periods