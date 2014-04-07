library(moments)

SP500data = read.csv("e:/dropbox/fnce 696/data/SP500_2013YTD.csv", header=TRUE)
GOOGdata = read.csv("e:/dropbox/fnce 696/data/GOOG.csv", header=TRUE)

n <- dim(SP500data)[1]
n
data1 <- SP500data[1:n,2]

#compute daily returns and annualize
rets = log(data1[2:n]/data1[1:(n-1)])
rets_annual = rets*252
print(c(mean(rets),mean(rets_annual)))

#calculate daily and annualized stddev
r_sd = sd(rets)
r_sd_annual = r_sd*sqrt(252)
print(c(r_sd,r_sd_annual))

r_var = var(rets)
r_var_annual = var(rets)*252
print(c(r_var,r_var_annual))

#higher order moments
#skewness is one tail is fatter than other (Assymmetry)
#fatter right(left) tail implies positive (negative) skewness
#normal distribution has skewness = 0
skewness(rets)
skewness(rnorm(10000))

#kurtosis means both tails are fatter than Normal distribution
#normal distribution has kurtosis = 3
kurtosis(rets)
kurtosis(rnorm(10000))