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
summary(res)

#signif codes = 1-% chance this is significant
#what's the probability a team will win based on set of stats?

x <- as.matrix(ncaa[,4:14])
y1 <- matrix(0,32,1)
y2 <- matrix(1,32,1)
y <- rbind(y2,y1)

library(MASS)

#DA
dm <- lda(y~x)
# fin Discriminant values that fit best, adjust data so is zero-mean, scale data by dividing by SD.
# D-score is now number of standard deviations
# output is group means and coefficients of a function

# what is the function?
dm$scaling	#use this to multiply by test characteristics to return D value

# predict using this model
dm$svd	#level of significance of this function, what is the value of the strongest eigenvalue

# which would be scored either way by the model?
predict(dm)$class

# compute confusion matrix
y_pred <- predict(dm)$class
amat <- table(y,y_pred)

# compute X^2 statistic. how significant is this?
emat <- matrix(c(16,16,16,16),nrow=2)

# generate X^2 statistic.  is this far enough away from zero?
test_stat <- sum((amat-emat)^2/emat)
1 - pchisq(test_stat,1)

# PRINCIPLE COMPONENT ANALYSIS
x2 <- ncaa[4:14]
res <- princomp(x) # what are the dimensions and how much of the result do they explain?
screeplot(res)
screeplot(res, type="lines")

summary(res)	#tells which data feed into each component
res$loadings	#what is each component comprised of?