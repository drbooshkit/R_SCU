library(quantmod)
library(reshape)


## DATA 1: BITCOIN
# Bring in bitcoin vs. USD time series. CSV file:
data = read.csv("e:/dropbox/fnce 696/data/bitcoin_dollar.csv", header=TRUE)

# convert first column to date type
dates <- as.Date(data[,1], "%m/%d/%Y")
data <- transform(data,
  Date <- as.Date(data$Date))
# Create a data frame with Date, Price, daily returns
n <- length(data$Date)
dailyRets <- log(data$Weighted.Price[2:n]/data$Weighted.Price[1:(n-1)])
dailyRets[2:n] <- dailyRets[1:(n-1)]
dailyRets[1] <- 0.0
data <- cbind(data, dailyRets)


## DATA 2: SPY
# SPY correlation
# use Quantmod to get other price time series
getSymbols(c("SPY"))
#date_range = "2007-01-01::2013-09-28"
#spy <- SPY[date_range]
# Create a data frame of adjusted close prices
spy <- data.frame(SPY[,6])

# calculate daily retuns and add that to Data Frame
n <- length(spy$SPY.Adjusted)
dailyRets2 <- log(spy$SPY.Adjusted[2:n]/spy$SPY.Adjusted[1:(n-1)])
dailyRets2[2:n] <- dailyRets2[1:(n-1)]
dailyRets2[1] <- 0.0
data2 <- cbind(spy, dailyRets2)
dates <- as.Date(rownames(data2), "%Y-%m-%d")
data2 <- cbind(Date=dates,data2)
data2$Date <- factor(data2$Date)	#match data type of Date from above

## Data 3: GOld
# GLD correlation
# use Quantmod to get other price time series
getSymbols(c("GLD"))
# Create a data frame of adjusted close prices
gld <- data.frame(GLD[,6])
# calculate daily retuns and add that to Data Frame
n <- length(gld$GLD.Adjusted)
dailyRets3 <- log(gld$GLD.Adjusted[2:n]/gld$GLD.Adjusted[1:(n-1)])
dailyRets3[2:n] <- dailyRets3[1:(n-1)]
dailyRets3[1] <- 0.0
data3 <- cbind(gld, dailyRets3)
dates <- as.Date(rownames(data3), "%Y-%m-%d")
data3 <- cbind(Date=dates,data3)
data3$Date <- factor(data3$Date)	#match data type of Date from above

## Data 4:  BTC_COST FROM CSV
data4 = read.csv("e:/dropbox/fnce 696/data/bitcoin_cost.csv", header=TRUE)
# convert first column to date type
data4$Date <- as.Date(data4[,1], "%m/%d/%y")
data4$Date <- factor(data4$Date)
#data4 <- transform(data4,
#  Date <- as.Date(data4$Date), "%m/%d/%y")
# Create a data frame with Date, Price, daily returns
n <- length(data4$BTC_cost)
dailyRets4 <- log(data4$BTC_cost[2:n]/data4$BTC_cost[1:(n-1)])
dailyRets4[2:n] <- dailyRets4[1:(n-1)]
dailyRets4[1] <- 0.0
data4 <- cbind(data4, dailyRets4)
data4$dailyRets4[data4$BTC_cost==0] <- 0.0	#replace zeros so zero is not NA


# Finally, create a new data frame with the asset prices and returns that matches up by date
total <- merge(data, data2, by="Date")	#merges dataframeA and dataframeB by ID
total <- merge(total, data3, by="Date")

total <- merge(total, data4, by="Date")


total <- rename(total, c(Weighted.Price="BTC", 
  dailyRets="BTCrets",SPY.Adjusted="SPY",dailyRets2="SPYrets", 
  GLD.Adjusted="GLD",dailyRets3="GLDrets", BTC_cost="BTC_cost", 
  dailyRets4="BTC_cost_delta"))


########
# PLOTS
#Set which is x data, which is y and add name labels
x <- total$Date
y <- total$BTC
y2 <- total$SPY
y3 <- total$GLD
y4 <- total$BTC_cost
main <- "Bitcoin-Equity Time Series"
xlab <- "Date"
ylab <- "Price, $"

# set graphical parameters to be used by default
# set line type, character type, symbol size, line weight
par(lwd=2, cex=1.5, font.lab=2)
par(cex.axis=1, cex.lab = 1.5, cex.main = 2.0)

# generate color array for plot
# specify colors in R by index, name, hexadecimal, RGB, or HSV. For example, col=1, col="white", col="#FFFFFF", col=rgb(1,1,1), and col=hsv(0,0,1)
#col <- c("red", "blue", "green")	
col <- rainbow(10)	#http://research.stowers-institute.org/efg/R/Color/Chart/

xmin <- min(x)
xmax <- max(x)
ymin <- min(y)
ymax <- max(max(y), max(y2), max(y3), max(y4))

plot(x, y, type="l",
pch=15, lty=1, ylim=c(ymin, ymax),
main=main, xlab=xlab, ylab=ylab, col=col[1], type="l")
lines(x, y2, type="l",
pch=16, lty=1, col=col[2])
lines(x, y3, type="l",
pch=17, lty=1, col=col[3])
lines(x, y4, type="l",
pch=18, lty=1, col=col[4])

library(Hmisc)
minor.tick(nx=4, ny=4, tick.ratio=0.5)
legend("right", inset=.05, title="Stocks", c("BTC","SPY","GLD","BTC Cost"),
lty=c(1, 2), pch=c(15, 17), col=col)


## returns plot and correlation
########
# PLOTS
#Set which is x data, which is y and add name labels
attach(total)
x <- Date
y <- BTCrets*100
y2 <- SPYrets*100
y3 <- GLDrets*100
y4 <- BTC_cost_delta*100
main <- "Bitcoin-Equity Daily Returns"
xlab <- "Date"
ylab <- "Daily Change, %"

# set graphical parameters to be used by default
# set line type, character type, symbol size, line weight
par(lwd=2, cex=1.5, font.lab=2)
par(cex.axis=1, cex.lab = 1.5, cex.main = 2.0)

# generate color array for plot
# specify colors in R by index, name, hexadecimal, RGB, or HSV. For example, col=1, col="white", col="#FFFFFF", col=rgb(1,1,1), and col=hsv(0,0,1)
#col <- c("red", "blue", "green")	
col <- rainbow(10)	#http://research.stowers-institute.org/efg/R/Color/Chart/

xmin <- min(x)
xmax <- max(x)
ymin <- -10 #min(y)
ymax <- 10 #max(max(y), max(y2), max(y3), max(y4))

plot(x, y, type="l",
pch=15, lty=1, ylim=c(ymin, ymax),
main=main, xlab=xlab, ylab=ylab, col=col[1], type="l")
lines(x, y2, type="l",
pch=16, lty=1, col=col[2])
lines(x, y3, type="l",
pch=17, lty=1, col=col[3])
lines(x, y4, type="l",
pch=18, lty=1, col=col[4])

library(Hmisc)
minor.tick(nx=4, ny=4, tick.ratio=0.5)
legend("right", inset=.05, title="Stocks", c("BTC","SPY","GLD","BTC Cost"),
lty=c(1, 2), pch=c(15, 17), col=col)

 
# Show correlation
rets <- data.frame(BTCrets, SPYrets, GLDrets, BTC_cost_delta)
pairs(rets)

# Test all correlations and p-values
library(psych)
corr.test(rets)

# The bivariate relationships underlying correlations can be visualized through scatter
# plots and scatter plot matrices, whereas correlograms provide a unique and powerful
# method for comparing a large numbers of correlation coefficients in a meaningful way.

# independent t-test
#library("MASS")
#t.test(rets$total.BTCrets ~ rets$total.GLDrets, data=rets)



#SCATTERPLOT
scatterplotMatrix(rets, spread=FALSE, diagonal="histogram", lty.smooth=2, main="BTC Daily Returns Correlation Plot")


#You can see from the figure that the highest correlations are between weight
#and displacement and weight and miles per gallon (red and closest to the principal
#diagonal).
library(gclus)
mydata <- rets
mydata.corr <- abs(cor(mydata))
mycolors <- dmat.color(mydata.corr)
myorder <- order.single(mydata.corr)
cpairs(mydata,
myorder,
panel.colors=mycolors,
gap=.5,
main="Variables Ordered and Colored by Correlation"
)


## CORRELOGRAM
# To interpret this graph, start with the lower triangle of cells (the cells below the principal
# diagonal). By default, a blue color and hashing that goes from lower left to upper
# right represents a positive correlation between the two variables that meet at that cell.
# Conversely, a red color and hashing that goes from the upper left to the lower right represents
# a negative correlation. The darker and more saturated the color, the greater the
# magnitude of the correlation. Weak correlations, near zero, will appear washed out.
# The upper triangle of cells displays the same information using pies. Here, color plays
# the same role, but the strength of the correlation is displayed by the size of the filled pie
# slice. Positive correlations fill the pie starting at 12 o’clock and moving in a clockwise
# direction. Negative correlations fill the pie by moving in a counterclockwise direction.
library(corrgram)
corrgram(rets, order=TRUE, lower.panel=panel.shade,
upper.panel=panel.pie, text.panel=panel.txt,
main="Correlogram of intercorrelations")
# Using elipses
corrgram(rets, order=TRUE, lower.panel=panel.ellipse,
upper.panel=panel.pts, text.panel=panel.txt,
diag.panel=panel.minmax,
main="Correlogram using scatter plots and ellipses")
# Just bottom triangle
corrgram(rets, lower.panel=panel.shade,
  upper.panel=NULL, text.panel=panel.txt,
  main="Correlogram (unsorted)")



## Numerical Corelation
#INITIAL, assume the varible don't interact with each other
fit <- lm(BTCrets ~ SPYrets + GLDrets + BTC_cost_delta, data=total)
summary(fit)
confint(fit)	#95% confidence intervals for the coefficient
#allow for interactions among predictor variables
#fit <- lm(BTCrets ~ SPYrets * GLDrets * AAPLrets, data=total)

#summary(fit)

#plot(fit) #evaluate model fit

# Global check for linear model assumptions
#library(gvlma)
#gvmodel <- gvlma(fit)
#summary(gvmodel)

# Check for multicollinearity
#vif(fit)
#sqrt(vif(fit)) > 2 # problem?


detach(total)