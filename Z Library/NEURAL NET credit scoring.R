#The avNNet function from the caret package fits a feed-forward neural network with one hidden layer. The network specified here contains three nodes (size=3) in the hidden layer. The decay parameter has been set to 0.1. The argument repeats=25 indicates that 25 networks were trained and their predictions are to be averaged. The argument linout=TRUE indicates that the output is obtained using a linear function. 

library(caret)

pairs(credit[,-(4:5)],diag.panel=panel.hist)

creditlog  <- data.frame(score=credit$score,
 log.savings=log(credit$savings+1),
 log.income=log(credit$income+1),
 log.address=log(credit$time.address+1),
 log.employed=log(credit$time.employed+1),
 fte=credit$fte, single=credit$single)
fit  <- avNNet(score ~ log.savings + log.income + log.address +
 log.employed, data=creditlog, repeats=25, size=3, decay=0.1,
 linout=TRUE)

##Neural Net Auto Regression
#fit <- nnetar(sunspotarea)
#plot(forecast(fit,h=20))