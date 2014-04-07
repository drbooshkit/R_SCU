data <- read.table(file="e:/datamining/BLS.txt",sep=",",header=FALSE)
ue <- data[,4]
plot(ue, type="l")
library("forecast")
fit <- auto.arima(ue)

ue <- data.frame(ue=ue, TTMForecast=seq(0,0,0))
# take trailing twelve months and estimate next month
for (i in 13:length(ue$ue)) {
	train <- auto.arima(ue$ue[(i-12):(i-1)])
	guess <- forecast(train,h=1)
	ue$TTMForecast[i] <- guess$mean[1]
}

ue <- ue[13:length(ue$ue),]
plot(ue$ue ~ ue$TTMForecast, xlab="Trailing Twelve Months 1-week forecast", ylab="Actual reported")


#plot(forecast(fit,h=20))

