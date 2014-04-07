# what is the normal distribution that maximizes probability of seeing this data in the normal distribution
# Column1: given x set of x1:xt
# what is the mean, mu, and std dev, sigma
# normal distribution function, probability density f(x)
# Column2: calculate f(xi) from f(x1) to f(xt)
# product of f(xi) is the total probability. but issues if f(xi) are small.
# take log of entire product. maximizing log product is same as maximizing product
# Column3: compute ln(f[xi]
# maximize the sum of column3 subject to changing mu and sigma



#what are the parameters to use for a given distribution



#probability integral up to x, from left. what's area from left to x. cumulative normal distribution
pnorm(0)
pnorm(1)

#height of curve at x (not the probability of x)
dnorm(0)

#generate a vector from x to y by step z.  seq(x,y,z)
x <- seq(-4,4,0.0001)

#compute integral of f(x)dx
sum(dnorm(x)*0.0001)

#what's "density function" of seeing X given average and stddev
dnorm(0.02,0.01,0.1)


#for each daily return, get density. calculate "joint" likelihood of all observations by multiplying string
#take the density of each daily return and take product
#find mean and sd that maximizes likelihood of capturing these returns
#use log to avoid computational issues.. "log likelihood"

#rets is column of returns, p is vector with mu and sig^2
#call maximizer to find best mean and sd

LL = function(params,rets) {
	alpha = params[1]; sigsq = params[2]
	logf = -log(sqrt(2*pi*sigsq))
			- (rets-alpha)^2/(2*sigsq)
LL = -sum(logf)
}
