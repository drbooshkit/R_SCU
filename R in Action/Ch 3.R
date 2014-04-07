# Working with graphs

#pdf("mygraph.pdf")	#print to PDF
  attach(mtcars)	#add this data frame to search paths, so i can call out data frame columns by just column name
  plot(wt, mpg)	#open a graphics window and generate scatter plot
  abline(lm(mpg~wt))	#regress mpg on wt and draw that line
  title("Regression of MPG on Weight")
  detach(mtcars)	
#dev.off()

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
data <-data.frame(dose=dose, drugA=drugA, drugB=drugB)

#Set which is x data, which is y and add name labels
x <- data$dose
y <- data$drugA
main <- "Plot Title"
xlab <- "X title"
ylab <- "Y title"

# set graphical parameters to be used by default
# set line type, character type, symbol size, line weight
par(lwd=2, cex=1.5, font.lab=2)
par(cex.axis=1, cex.lab = 1.5, cex.main = 2.0)

# generate color array for plot
# specify colors in R by index, name, hexadecimal, RGB, or HSV. For example, col=1, col="white", col="#FFFFFF", col=rgb(1,1,1), and col=hsv(0,0,1)
col <- c("red", "blue", "green")	
col <- rainbow(10)	#http://research.stowers-institute.org/efg/R/Color/Chart/

xmin <- min(x)
xmax <- max(x)
ymin <- min(y)
ymax <- max(y)

plot(x, y, type="b",
pch=15, lty=1, col="red", ylim=c(ymin, ymax),
main=main,
xlab=xlab, ylab=ylab)
lines(x, data$drugB, type="b",
pch=17, lty=2, col="blue")
abline(h=c(30), lwd=1.5, lty=2, col="gray")
library(Hmisc)
minor.tick(nx=3, ny=3, tick.ratio=0.5)
legend("topleft", inset=.05, title="Drug Type", c("A","B"),
lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))




# Example of a 2x2 bunch of graphs
#attach(mtcars)
#opar <- par(no.readonly=TRUE)
#par(mfrow=c(2,2))
#plot(wt,mpg, main="Scatterplot of wt vs. mpg")
#plot(wt,disp, main="Scatterplot of wt vs disp")
#hist(wt, main="Histogram of wt")
#boxplot(wt, main="Boxplot of wt")
#par(opar)
#detach(mtcars)

# Create a layout matrix
#attach(mtcars)
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
#hist(wt)
#hist(mpg)
#hist(disp)
#detach(mtcars)