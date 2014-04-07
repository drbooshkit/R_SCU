#given string stock symbol stock
#given string date range string dates

get_rets <- function(stock,date_range) {
	
	library(quantmod)
	stock <- getSymbols(Symbols=stock, auto.assign=FALSE)

	stock <- stock[date_range]
	print(stock)
	#get last column, closing price
	stock <- as.numeric(stock[,6])

	#compute returns between one day and the next, continuously compounding
	#we care about returns, not prices typically
	n <- length(stock)[1]
	rets <- (log(stock[2:n]/stock[1:(n-1)]))

	return(rets)
}
