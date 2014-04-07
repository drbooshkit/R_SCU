data(iris)

# run naive bayes
# get confusion matrix

# run SVM
library(e1071)

# pass training data to SVM
res <- svm(iris[,1:4],iris[,5])

# see how well prediction with SVM works
out <- table(predict(res, iris[,1:4]), iris[,5])

# predict one flower
predict(res, iris[6,1:4])

