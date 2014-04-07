Tdata <- read.csv(file="e:/datamining/finance/treasury_data.csv", header=TRUE)

nBonds <- dim(Tdata)[1]

# for (i in nBonds) {
CFtable <- NULL
CFtable <- data.frame(CF = numeric(1), TTCF = numeric(1), CFT = numeric(1), CFTT = numeric(1), CFTTT = numeric(1), taoI = integer(1), tMinusTao = numeric(1))
CFtableTemp <- data.frame(CF = numeric(1), TTCF = numeric(1), CFT = numeric(1), CFTT = numeric(1), CFTTT = numeric(1), taoI = integer(1), tMinusTao = numeric(1))

i <- 1	

tao <- 0.5  # set tao


workData <- Tdata[1,]
CFtable$CF[1] <- workData$CouponPayment+100
CFtable$TTCF[1] <- workData$TTM
CFtable$taoI[1] <- if(CFtable$TTCF[1]>tao) 1 else 0

x <- 0
while (x == 0) {
PrevCF <- CFtable$TTCF[1] - 0.5
if (PrevCF < 0) x <- 1 # and exit the loop
CFtableTemp$TTCF[1] <- PrevCF
CFtableTemp$CF[1] <- workData$CouponPayment[1]
rbind(CFtable, CFtableTemp)

# if t > tao, I=1

}

CFtable$CFT <- CFtable$CF*CFtable$TTCF
CFtable$CFTT <- CFtable$CF*CFtable$TTCF^2
CFtable$CFTTT <- CFtable$CF*CFtable$TTCF^3






CFtable <- rbind(CFtable, 


	