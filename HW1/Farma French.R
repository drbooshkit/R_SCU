#http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html

library(quantmod)
library(MASS)
library(car)
library(psych)	# needed for corr.test()
library(rt1le)		#cor() 
library(ellipse)	#plotcorr() function
library(gclus)
library(corrgram)


#Download the three Fama-French factors, i.e., {Rm-Rf, SMB, HML}. Use weekly data. 

# Bring in CSV file:
data = read.csv("e:/dropbox/fnce 696/data/FFF.csv", header=TRUE)
# convert first column to date type. # Describe
dates <- as.Date(data[,1], "%m/%d/%y")
data <- transform(data,
  Date <- as.Date(data$Date))
summary(data)


# Create a layout of four histograms
# Plot all Farma French Factor distribution
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

# Generalized histogram function
zplot <- function(var) {
	hs <- hist(eval(parse(text=var)), main ="",	#use parse(text=var) instead of actual name
	xlab=var, ylab="Frequency",
	col="grey90", breaks="fd", 
	border=TRUE)
	dens <- density(eval(parse(text=var)), na.rm=TRUE)
	rs <- max(hs$counts)/max(dens$y)
	lines(dens$x, dens$y*rs, type="l", col=rainbow(1)[1])
	rug(eval(parse(text=var)))	#add a rug to the plot
	title(main=paste("Distribution of",var))	#add title
}

# call on 4 plots
t1ach(data)
par(mfrow=c(2,2))
v <- c("Mkt.RF","SMB","HML","RF")
lapply(v, zplot)
detach(data)

#(a) Prepare the correlation matrix of these three data series
t1ach(data)
rets <- data.frame(Mkt.RF, SMB, HML)	#take only numerical data to a new frame
cor <-cor(rets, use="pairwise", method="pearson")	
cor	#show correlation matrix
# Weak Ellipses
par(mfrow=c(1,1))
plotcorr(cor, col=rainbow(c(1,2,3)))

# Sct1er Plot
sct1erplotMatrix(rets, spread=FALSE, diagonal="histogram", lty.smooth=2, main="Correlation Plot")
corr.test(rets)

# Using elipses
corrgram(rets, order=TRUE, lower.panel=panel.ellipse,
upper.panel=panel.pts, text.panel=panel.txt,
diag.panel=panel.minmax,
main="Correlogram using sct1er plots and ellipses")



### add in stock data

stk <- "T"

#b) Download the stock series of your favorite stock from any site you like. 
getSymbols(stk)
data2 <- to.weekly(T)
t1 <- data.frame(T[,6])
dates2 <- as.Date(rownames(t1), "%Y-%m-%d")
t1 <- cbind(Date=dates2,t1)
t1$Date <- factor(t1$Date)	#match data type of Date from above
n <- length(t1$T.Adjusted)
weeklyRets <- log(t1$T.Adjusted[2:n]/t1$T.Adjusted[1:(n-1)])
weeklyRets[2:n] <- weeklyRets[1:(n-1)]
weeklyRets[1] <- 0.0
t1$Trets <- weeklyRets


# create total data for linear model
total <- merge(data,t1, by="Date")

attach(total)
test <- data.frame(Mkt.RF, SMB, HML, Trets)
detach(total)

#(c) Run a regression of the weekly *return* on your stock against the Rm-Rf variable for the matching period. 
#Choose any period you like, but use at least five years of data. 
#Describe the regression output in detail. 
#t.test(Mkt.RF ~ Trets, data=test)

# Create linear fit
fit <- lm(Trets ~ Mkt.RF, test)
summary(fit)
confint(fit, level=0.8)

# Plot residuals
res <- residuals(fit)
plot(res)
abline(0,0)



#(d) Now run a regression of the return of your stock against all three Fama French factors. What is different?
fit <- lm(Trets ~ Mkt.RF + SMB + HML, test)
summary(fit)
confint(fit, level=0.8)
# Plot residuals
res <- fit$residuals
abline(0,0)

# Forecast Trets  using factors)
#library(fpp)
#fit <- lm(Trets ~ Mkt.RF + SMB + HML, test)
#fcast <- forecast(fit, h=20)
#plot(fcast)

sct1erplotMatrix(test, spread=FALSE, diagonal="histogram", lty.smooth=2, main="Correlation Plot")
corr.test(test)


mydata <- test
mydata.corr <- abs(cor(mydata))
mycolors <- dmat.color(mydata.corr)
myorder <- order.single(mydata.corr)
#cpairs(mydata,
#myorder,
#panel.colors=mycolors,
#gap=.5,
#main="Variables Ordered and Colored by Correlation"
#)

# Using elipses
#corrgram(test, order=TRUE, lower.panel=panel.ellipse,
#upper.panel=panel.pts, text.panel=panel.txt,
#diag.panel=panel.minmax,
#main="Correlogram using sct1er plots and ellipses")
# Just bottom triangle 
#corrgram(test, lower.panel=panel.shade,
#  upper.panel=NULL, text.panel=panel.txt,
#  main="Correlogram (unsorted)")



### Hierarchical clustering
cc <- cor(test, use="pairwise", method="pearson")
hc <- hclust(dist(cc),method="average")

dn <- as.dendrogram(hc)
plot(dn, horiz=TRUE, nodePar = list(col = 3:2, cex = c(2.0,0.75),
	pch=21:22, bg = c("light blue", "pink"), lab.cex = 0.75, lab.col="tomato"),
	edgePar = list(col="gray", lwd=2), xlab = "Height")
	title(main="Correlation Clusters using Pearson Method")
	sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"))
	par(op)
	
# smaller lines = tighter correlation