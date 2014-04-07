x <- c(3.0,6.0,1.0,2.0,2.0)

dpois(x, lambda=1)

sum(log(dpois(x,lambda=1)))

poisson.LL<-function(lambda) -sum(log(dpois(x,lambda)))

res <- nlm(poisson.LL, 1.0)

z <- dpois(x, lambda=2.8)
