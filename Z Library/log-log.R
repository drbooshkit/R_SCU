library(forecast)

data = read.csv("e:/datamining/forecasting/800m.csv", header=TRUE)

fit <- lm(log(Time) ~ log(Year), data=data)
plot(Time ~ Year,data=data)
lines(1900:2000, exp(fit$coefficients[1]+fit$coefficients[2]*log(1900:2000)))


abline(fit)

fit