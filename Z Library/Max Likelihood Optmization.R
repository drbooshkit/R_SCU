# create a function that multiplies n numbers together

getSymbols("GOOG")
goog <- as.numeric(GOOG[,6])
n <- length(goog)

rets = (goog[2:n] - goog[1:(n-1)])/goog[1:(n-1)]


logLL <- function(p,rets) {
	mu <- p[1]
	sig <- p[2]
	fr <- exp(-0.5*(rets-mu)^2/sqrt(2*pi*sig^2))
	result <- -1*sum(log(fr))
}

# call minimizer to maximize the negative
p <- c(0.01,0.01)
logLL(p,rets)

# nonlinear minimizer (to minimize the negative maximum)
res <- nlm(logLL, p, rets)