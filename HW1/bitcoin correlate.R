library(quantmod)
library(reshape)

# Bring in bitcoin vs. USD time series. CSV file:
data = read.csv("e:/dropbox/fnce 696/data/bitcoin_dollar.csv", header=TRUE)

# convert first column to date type
dates <- as.Date(data[,1], "%m/%d/%Y")
data <- transform(data,
  Date <- as.Date(data$Date))
# Create a data frame with Date, Price, daily returns
n <- length(data$Date)
dailyRets <- log(data$Weighted.Price[2:n]/data$Weighted.Price[1:(n-1)])
dailyRets[2:n] <- dailyRets[1:(n-1)]
dailyRets[1] <- 0.0
data <- cbind(data, dailyRets)


# GOOGLE correlation
# use Quantmod to get other price time series
getSymbols(c("GOOG"))
#date_range = "2007-01-01::2013-09-28"
#goog <- GOOG[date_range]
# Create a data frame of adjusted close prices
goog <- data.frame(goog[,6])

# calculate daily retuns and add that to Data Frame
n <- length(goog$GOOG.Adjusted)
dailyRets2 <- log(goog$GOOG.Adjusted[2:n]/goog$GOOG.Adjusted[1:(n-1)])
dailyRets2[2:n] <- dailyRets2[1:(n-1)]
dailyRets2[1] <- 0.0
data2 <- cbind(goog, dailyRets2)
dates <- as.Date(rownames(data2), "%Y-%m-%d")
data2 <- cbind(Date=dates,data2)
data2$Date <- factor(data2$Date)	#match data type of Date from above

# Finally, create a new data frame with the asset prices and returns that matches up by date
total <- merge(data, data2, by="Date")	#merges dataframeA and dataframeB by ID
total <- rename(total, c(Weighted.Price="BTC", dailyRets="BTCrets",GOOG.Adjusted="GOOG",dailyRets2="GOOGrets"))

with(total)
  fit <- lm(BTCrets ~ GOOGrets, total)


