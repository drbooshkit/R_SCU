library("plyr")library("twitteR")
library("ROAuth")
library("RCurl")
library("tm")

## resoution for SSL verification issue - disable it
opts <- list(
  capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), 
  ssl.verifypeer = FALSE);
options(RCurlOptions = opts)


### SIMPLE HARVARD POS NEGATIVE DICTIONARY
# Read in HI Dictionary from TXT  #every word is tagged by psychometric tags
HIDict = readLines("e:/Dropbox/FNCE 696/Data/inqdict.txt")	
# create a dictionary of Pos-tagged words
dict_pos <- HIDict[grep("Pos",HIDict)]	#return index for every line that contains "Pos"
poswords <- NULL
for (s in dict_pos) {
	s <- strsplit(s,"#")[[1]][1]		#split at hash to remove instance ID of same words
	poswords <- c(poswords, strsplit(s," ")[[1]][1])	#returns list, take first element of that array
}
poswords <- tolower(poswords)
# create a dictionary of Neg-tagged words
dict_neg <- HIDict[grep("Neg",HIDict)]	#return index for every line that contains "Pos"
negwords=NULL
for (s in dict_neg) {
	s <- strsplit(s,"#")[[1]][1]		#split at hash to remove instance ID of same words
	negwords <- c(negwords, strsplit(s," ")[[1]][1])	#returns list, take first element of that array
}
negwords <- tolower(negwords)
######################


#### AUTHORIZATION CODE
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
cKey <- "qWamhfy2AktHXIKd7meKZA"
cSecret <- "pgo6peFO6QySDaRAjtjUk793cbayBukO0IQlI8t2UVM"
reqURL <- "https://api.twitter.com/oauth/request_token"
authURL <- "https://api.twitter.com/oauth/authorize"
accURL <- "https://api.twitter.com/oauth/access_token"
cred <- OAuthFactory$new(consumerKey=cKey, consumerSecret=cSecret,requestURL=reqURL,
	accessURL=accURL,authURL=authURL)
cred$handshake(cainfo="cacert.pem")
registerTwitterOAuth(cred)
save(list="cred", file="twitteR_credentials")
load("twitteR_credentials")
registerTwitterOAuth(cred)



#### some examples of twitter searches, retrieving m tweets
m <- 1000

# search for m tweets based on string
#searchTwitter("#beer", n=m)
tweets <- searchTwitter("#GAP", n=m)

## Search between two dates
#searchTwitter("charlie sheen", since="2011-03-01", until="2011-03-02")

## geocoded results
#tweets <- searchTwitter("big data", geocode="37.3628,-121.9292,50mi", n=m)

# get m tweets from user
#user <- "WSJ" 
#userInfo <- getUser(user,cainfo="cacert.pem") #Works correctly
#tweets <- userTimeline(user, n=m,cainfo="cacert.pem")

# create a data frame
tweets_df = twListToDF(tweets)

n <- length(tweets_df$created)
y <- seq(1,n)
y <- y[n:1]
plot(y ~ tweets_df$created, main="Tweets over Time, #shutdown", xlab="Date and Time",ylab="Cumulative Tweets")

# what is the time window for which we got tweets?
timeWindow <- difftime(max(tweets_df$created),min(tweets_df$created),units="hours")
# what is the average frequency of tweets per hour
tweetFreq <- length(y)/as.numeric(timeWindow)


# pull out only text from twitter data frame
tweets <- lapply(tweets, function(t)t$getText())


Score <- NULL

# score tweets for sentiment
for (i in 1:m) {
	text <- tweets[i]
	txtCLEAN <- tolower(text)
	txtCLEAN <- removePunctuation(txtCLEAN)
	txtCLEAN <- strsplit(txtCLEAN, " ")
	txtCLEAN <- unlist(txtCLEAN)
	
## POSITIVE WORD COUNT
posmatch <- match(txtCLEAN, poswords)	#take two vectors/arrays and give back matches
numPosMatch <- length(posmatch[which(posmatch>0)])

#### NEGATIVE WORD COUNT
negmatch <- match(txtCLEAN, negwords)
numNegMatch <- length(negmatch[which(negmatch>0)])

Score[i] <- (numPosMatch - numNegMatch)/(numPosMatch + numNegMatch)
#print(Score[i])
}

# Remove NaN results
Score <- na.omit(Score)
# plot distribution of polarity score
hist(Score, xlab="Positive/Negative Polarity, Normalized", ylab="Count of Tweets", main="Twitter Polarity Distribution")
mean(Score)

m <- length(Score)
Score <- Score[m:1]

netScore <- NULL

for (n in 1:length(Score)) {
netScore[n] <- sum(Score[1:n])
}
plot(netScore, type="l", main="Trend of Cumulative Sentiment over Tweets", xlab="Number of Tweets", ylab="Cumulative Sentiment Score")