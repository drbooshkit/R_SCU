#read raw txt of playoff data, 64 teams
ncaa = read.table("e:/dropbox/fnce 696/data/ncaa.txt", header=TRUE)

#what defines the better teams? what performance characteristics?
#1. run simple regression, which variables explain data?

# number of games = progress = performance = column 3
y = ncaa[3] #LHS
y = as.matrix(y,64,1)
x = ncaa[,4:14] #RHS
x = as.matrix(x,64,11)

res = lm(y~x)  #regress y on x
library(lmtest)

# test for heteroskedasticity using Breush-Pagan test
# look at F-statistic
bptest(res)

library(car)
vb = hccm(res)


# standard deviations of betas in correlation matrix
stdb = sqrt(diag(vb))
stdb

#take beta correlations, then divide this by the std deviations of beta
b = res$coefficients
res_corrected = b/stdb
res_corrected
