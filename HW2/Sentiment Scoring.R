
library(tm)
library(wordcloud)
library(RCurl)
library(XML)


htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}


getSent <- function(targetURL, trainIN) {

# call the htmlToText routine on the current URL
txtCLEAN <- htmlToText(targetURL)

# take ONE STRING and split into list and then flatten
txtCLEAN <- strsplit(txtCLEAN, " ")
txtCLEAN <- unlist(txtCLEAN)

# convert to lower case for consistency
txtCLEAN <- tolower(txtCLEAN)

# convert to Corpus and RDM and do the count. create a dataframe. remove stopwords
alllib <- Corpus(VectorSource(txtCLEAN))
tdm <- TermDocumentMatrix(alllib, control=list(stopwords=TRUE))
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)

# take matches between training dictionary and text
# adjust this trainIN column per training text format
wordMatch <- match(txtCLEAN, trainIN[,1])

# how many words did we find?
numMatch <- length(wordMatch[which(wordMatch>0)])	

# report back in a new vector the words that matched and also sum the sentiment score
matchLib <- trainIN[wordMatch[which(wordMatch>0)],1]
matchValue <- sum(trainIN[wordMatch[which(wordMatch>0)],2])

# convert to Corpus and RDM and do the count. create a dataframe with words and frequencies
matchLib <- Corpus(VectorSource(matchLib))
tdm <- TermDocumentMatrix(matchLib, control=list(stopwords=TRUE))
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)

# add a scored column which is frequency * sentiment score
wordMatch2 <- match(d$word, train$V1)
d$score <- d$freq*train$V2[wordMatch2]
d$score2 <- sqrt(d$score^2)	# take absolute value for wordcloud output

# PDF Output of Wordcloud using score (instead of frequency) to show size to show weights
#z <- paste("Sentiment Words.PDF")	# how to add intelligent URL here to file name???
#pdf(file=z)
#wordcloud(d$word, d$score2, max.words=200, random.color=TRUE, min.freq=1, random.order=TRUE)
#dev.off()

# calculate average sentiment per word scored
avgSent1 <- sum(d$score)/length(matchLib)
#print(sum(d$score))
#print(c(length(matchLib)),"words matched")
return(avgSent1)

}


#### END HELPER FUNCTIONS




### BEGIN MAIN CODE
## read website and return html
#text <- readLines("http://www.wsj.com")

## CREATE LIST OF NEWS SITES HERE
#news_sites <- c("http://fulltextrssfeed.com/www.npr.org/rss/rss.php?id=1001", "http://www.wsj.com", "http://news.google.com/news",
#    "http://fulltextrssfeed.com/rss.nytimes.com/services/xml/rss/nyt/HomePage.xml",  "http://fulltextrssfeed.com/feeds.reuters.com/Reuters/domesticNews", 
#  "http://fulltextrssfeed.com/feeds.reuters.com/Reuters/worldNews", "http://news.google.com/?output=rss", "http://abcnews.go.com", "http://www.nbcnews.com", "http://www.cbsnews.com")

news_sites <- c("http://web.archive.org/web/20131008173557/http://online.wsj.com/home-page",
"http://web.archive.org/web/20131007123641/http://online.wsj.com/home-page",
"http://web.archive.org/web/20131006210426/http://europe.wsj.com/home-page",
"http://web.archive.org/web/20131005110806/http://online.wsj.com/home-page",
"http://web.archive.org/web/20131004112226/http://online.wsj.com/home-page",
"http://web.archive.org/web/20131003074125/http://online.wsj.com/home-page",
"http://web.archive.org/web/20131002103332/http://online.wsj.com/home-page",
"http://web.archive.org/web/20131001085821/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130930235920/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130929115953/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130928091852/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130926094354/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130925092033/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130924065714/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130923185851/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130922062202/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130921054213/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130920083346/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130919032007/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130918042114/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130917033030/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130916091150/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130915040458/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130914021146/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130913042405/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130912023916/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130911221739/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130910042731/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130909162723/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130908055659/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130907171236/http://online.wsj.com/home-page",
"http://web.archive.org/web/20130906043738/http://online.wsj.com/home-page")


####### sentiment analysis using AFINN-111 sentiment tagged dictionary ######
# Bring in AFINN-111 Training dictionary
train <- read.delim("e:/datamining/datasets/afinn/afinn-111.txt", header=FALSE)

# resent score array
avgSent <- NULL

# get averaged Sentiment score for each site
for (i in 1:length(news_sites)) {
	
avgSent[i] <- getSent(news_sites[i], train)
print(news_sites[i])
print(avgSent[i])

}

# what is overall mean Sentiment?
print(mean(avgSent))

# what is distribution of Sentiment per site?
hist(avgSent)

