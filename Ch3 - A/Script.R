library(quantmod)
getSymbols(c("GOOG","AAPL","CSCO","IBM"))
date_range = '2007-01-01::2011-03-31'

goog = GOOG[date_range]
aapl = AAPL[date_range]
csco = CSCO[date_range]
ibm = IBM[date_range]

##Concatenate columns of data into one stock data set:
goog = as.numeric(goog[,6])
aapl = as.numeric(aapl[,6])
csco = as.numeric(csco[,6])
ibm = as.numeric(ibm[,6])
stkdata = cbind(goog,aapl,csco,ibm)
dim(stkdata)

#compute daily returns (log)
rets = log(stkdata[2:1070,]/stkdata[1:1069,])
colMeans(rets)

#compute covariance matrix and correlation matrix (printing with ,2 sig digits)
cv = cov(rets)
print(cv,2)
cr = cor(rets)
print(cr,4)









#take the inverse of the COV matrix
#cv_inv = solve(cv)
#print(cv_inv,3)

#veritfy inverse times itself result in identity matrix (all 1's on diagonals)
#print(cv_inv %*% cv,3)

#check for positive definite

#library(corpcor)
#is.positive.definite(cv)
#is.positive.definite(x)
#is.positive.definite(x %*% t(x))

####Matrices
#fill 4x3 matrix with normally(?)-distributed random numbers
#x = matrix(rnorm(12),4,3)
#print(x,3)
#create transposed matrix
#print(t(x),3)

#multiply conforming matrices A*B. # of columns in A must be # of rows in B.  resultant has # of rows of A and # of columns in B
#print(x %*% t(x),3)
#print(t(x) %*% x,3)