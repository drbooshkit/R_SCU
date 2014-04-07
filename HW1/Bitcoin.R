library(moments)

#Visit the Quandl web site "www.quandl.com". (This is an amazing data repository!) Download the time series of bitcoin exchange rates versus US dollars. 
#(a) What is bitcoin?

#read in CSV file:
data = read.csv("e:/dropbox/fnce 696/data/bitcoin_dollar.csv", header=TRUE)
bitcoin = data[,2]
n <- length(bitcoin)

#(b) What are the rates of return characteristics of bitcoin? 
daily_rets = log(bitcoin[2:n]/bitcoin[1:(n-1)])

#what's the average daily return
mean(daily_rets)

#what's the average annualized return:
annual_rets = daily_rets*252
print(c(mean(daily_rets),mean(annual_rets)))

#calculate daily and annualized stddev
r_sd = sd(daily_rets)
r_sd_annual = r_sd*sqrt(252)
print(c(r_sd,r_sd_annual))

r_var = var(daily_rets)
r_var_annual = var(daily_rets)*252
print(c(r_var,r_var_annual))

#higher order moments
#skewness is one tail is fatter than other (Assymmetry)
#fatter right(left) tail implies positive (negative) skewness
#normal distribution has skewness = 0
#skewness(rets)

#kurtosis means both tails are fatter than Normal distribution
#normal distribution has kurtosis = 3
#kurtosis(rets)

#plot distribution of daily returns
#hist(daily_rets)
#plot(daily_rets,xlab="Time, days")


#How much return would you have made (annualized) from holding bitcoin over varied intervals? 



n_max_hold <- length(daily_rets)  # what's the most days you could hold this?
n_hold <-1 #how many days do you hold it?
buy_day <- 1	#from 1 to (n_max_hold-1), how long do you hold the bitcoin asset?
max_ret <- 0.0

return_matrix <- matrix(ncol=3)

for(buy_day in 1:(n_max_hold-1)) {			#for each possible day you could buy
	for(n_hold in 1:(n_max_hold-buy_day))	{	# for each possible length of hold
		this_ret <- 1.0							#reset holding period return to 1
		for(j in 1:n_hold) {					#calculate cumulative Return by multiplying 1 * (1+next day's return) 
			this_ret <- ((1+daily_rets[j+buy_day-1]) * this_ret)			
}

		return_matrix <- rbind(return_matrix,c(buy_day,n_hold,this_ret))    #each possible scenario, create a row in a resuly matrix
		
		if (this_ret > max_ret) {									#if there's a new max, print it for us to see. status check.
			max_ret <- this_ret
			print(c(buy_day,n_hold,max_ret))			
		}
	}
}


x1 <- return_matrix[,1]
x2 <- return_matrix[,2]
y <- return_matrix[,3]

par(mfrow=c(2,2))
plot(x1, y, main="Scatterplot of Return vs. Day Bought")
plot(x2, y, main="Scatterplot of Return vs. Days Held")
hist(y, main="Histogram of Returns")
boxplot(y, main="Boxplot of Returns")

