library("twitteR")
library("ROAuth")
library("RCurl")



download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#log in as user, then get dev access

cKey <- "qWamhfy2AktHXIKd7meKZA"
cSecret <- "pgo6peFO6QySDaRAjtjUk793cbayBukO0IQlI8t2UVM"

reqURL <- "https://api.twitter.com/oauth/request_token"
authURL <- "https://api.twitter.com/oauth/authorize"
accURL <- "https://api.twitter.com/oauth/access_token"
oauth_token <- "84407302-LBpun7FpnznQ4G4e7wr522m8ZjnTtNLgHjzF4uwXq"
oauth_token_secret <- "VL0E8UtbyUOkylDOYp1LFZaOOx56rGpvdBbzlthN8"

cred <- OAuthFactory$new(consumerKey=cKey, consumerSecret=cSecret,requestURL=reqURL,
	accessURL=accURL,authURL=authURL)
#cred <- OAuthFactory$new(requestURL=reqURL,
#	accessURL=accURL,authURL=authURL, oauth_token=oauth_token, oauth_token_secret=oauth_token_secret)

cred$handshake(cainfo="cacert.pem")

registerTwitterOAuth(cred)

save(list="cred", file="twitteR_credentials")
library(twitteR)
load("twitteR_credentials")
registerTwitterOAuth(twitCred)


#s = searchTwitter('#GOOG', n = 500,cainfo="cacert.pem") #most recent 500
#sNewTweets = searchTwitter('#GOOG', n = 500,cainfo="cacert.pem",sinceID=388976371810455552)  #tweets since ID
#sTweetDates = searchTwitter('#GOOG', n = 500,since='2013-10-04',until='2013-10-05',cainfo="cacert.pem")	#tweets between Dates

### SIMPLE HARVARD POS NEGATIVE DICTIONARY
# Read in HI Dictionary from TXT  #every word is tagged by psychometric tags
HIDict = readLines("e:/Dropbox/FNCE 696/Data/inqdict.txt")	


# get friends
srdas$getFriends(n=6, cainfo="cacert.pem")


# get n tweets from user
m <- 476
user <- "srdas" 
userInfo <- getUser(user,cainfo="cacert.pem") #Works correctly
s_tweets <- userTimeline(user, n=m,cainfo="cacert.pem")

for (i in 1:m) {
	text <- s_tweets[[i]]$text
	txtCLEAN <- tolower(text)
	txtCLEAN <- strsplit(txtCLEAN, " ")
	txtCLEAN <- unlist(txtCLEAN)

## POSITIVE WORD CLOUD
# create a dictionary of Pos-tagged words
dict_pos <- HIDict[grep("Pos",HIDict)]	#return index for every line that contains "Pos"

poswords=NULL
for (s in dict_pos) {
	s <- strsplit(s,"#")[[1]][1]		#split at hash to remove instance ID of same words
	poswords <- c(poswords, strsplit(s," ")[[1]][1])	#returns list, take first element of that array
}
poswords <- tolower(poswords)
# match to list# Create semantic word clouds and export them to PNG
posmatch <- match(txtCLEAN, poswords)	#take two vectors/arrays and give back matches
numPosMatch <- length(posmatch[which(posmatch>0)])

#### NEGATIVE WORD CLOUD
# create a dictionary of Neg-tagged words
dict_neg <- HIDict[grep("Neg",HIDict)]	#return index for every line that contains "Pos"
negwords=NULL
for (s in dict_neg) {
	s <- strsplit(s,"#")[[1]][1]		#split at hash to remove instance ID of same words
	negwords <- c(negwords, strsplit(s," ")[[1]][1])	#returns list, take first element of that array
}
negwords <- tolower(negwords)
negmatch <- match(txtCLEAN, negwords)
numNegMatch <- length(negmatch[which(negmatch>0)])

Score[i] <- (numPosMatch - numNegMatch)/(numPosMatch + numNegMatch)
print(Score[i])
}

hist(Score, xlab="Positive/Negative Polarity, Normalized", ylab="Count of Tweets", main="Twitter User Polarity Distribution")
mean(Score)