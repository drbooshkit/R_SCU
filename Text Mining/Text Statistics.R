library(e1071)

data(iris)	#example data set, predict which flower by recognition algorithm

# Pass data to Bayes algorithm
# training data set
# what is the distribution of each characteristic (mean and standard deviation)?
# given those 4 distributions, what are the probabilities of each?

res <- naiveBayes(iris[,1:4],iris[,5])	
res	#these are the prior probabilities


# given these 4 parameters of a flower, what is the probability it is a certain type?  compute bayesian conditional probabilities
predict(res, iris[3,1:4],type="raw")	#run on the third flower and return back conditional probabilities given these 4 characteristics

# how is the in-sample performance?
out = table(predict(res, iris[,1:4]), iris[,5])	#compare training data to actual answer
print(out)
# this is called a "confusion matrix" how confused is the algorithm? HEAVY diagonal is good 

# how is out of sample performance?