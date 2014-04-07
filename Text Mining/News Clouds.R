
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


## read website and return html
#text <- readLines("http://www.wsj.com")

## CREATE LIST OF NEWS SITES HERE
news_sites <- c("http://fulltextrssfeed.com/www.npr.org/rss/rss.php?id=1001", "http://www.wsj.com", "http://news.google.com/news",
	 "http://fulltextrssfeed.com/www.foxnews.com/about/rss/", "http://www.c-span.org/", 
	"http://fulltextrssfeed.com/rss.nytimes.com/services/xml/rss/nyt/HomePage.xml",  "http://fulltextrssfeed.com/feeds.reuters.com/Reuters/domesticNews", 
	"http://fulltextrssfeed.com/feeds.reuters.com/Reuters/worldNews", "http://news.google.com/?output=rss")


# visit each news site and get clean text string
alltxt <- NULL
for (i in 1:length(news_sites)) {
# call the htmlToText routine
txtCLEAN <- htmlToText(news_sites[i])
# take ONE STRING and split into list and then flatten
txtCLEAN <- strsplit(txtCLEAN, " ")
txtCLEAN <- unlist(txtCLEAN)
alltxt <- c(alltxt, txtCLEAN)
length(alltxt)
} 


txtCLEAN <- alltxt
txtCLEAN <- tolower(txtCLEAN)

# convert to Corpus and RDM and do the count. create a dataframe. remove stopwords
alllib <- Corpus(VectorSource(txtCLEAN))
tdm <- TermDocumentMatrix(alllib, control=list(stopwords=TRUE))
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)







### DICTIONARY
# Read in HI Dictionary from TXT  #every word is tagged by psychometric tags
HIDict = readLines("e:/Dropbox/FNCE 696/Data/inqdict.txt")	

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
poslib <- poswords[posmatch[which(posmatch>0)]]

# convert to Corpus and RDM and do the count. create a dataframe
poslib <- Corpus(VectorSource(poslib))
tdm <- TermDocumentMatrix(poslib, control=list(stopwords=TRUE))
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)

# PDF Output of Wordcloud
pdf('PositiveWords.pdf')
wordcloud(d$word, d$freq, max.words=200, random.color=TRUE, min.freq=1, random.order=TRUE)
dev.off()

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
#numNegMatch <- length(negmatch[which(negmatch>0)])
neglib <- negwords[negmatch[which(negmatch>0)]]

# convert to Corpus and RDM and do the count. create a dataframe
neglib <- Corpus(VectorSource(neglib))
tdm <- TermDocumentMatrix(neglib, control=list(stopwords=TRUE))
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)
# PDF Output of Wordcloud
pdf('NegativeWords.pdf')
wordcloud(d$word, d$freq, max.words=200, random.color=TRUE, min.freq=1, random.order=TRUE)
dev.off()


#### ALL WORDS WORD CLOUD
# create a dictionary with desired tag
dict_econ <- HIDict[grep("ECON",HIDict)]	

econwords <- NULL
for (s in dict_econ) {
	s <- strsplit(s,"#")[[1]][1]		#split at hash to remove instance ID of same words
	econwords <- c(econwords, strsplit(s," ")[[1]][1])	#returns list, take first element of that array
}
econwords <- tolower(econwords)
econmatch <- match(txtCLEAN, econwords)
a <- econwords[econmatch[which(econmatch>0)]]
# convert to Corpus and RDM and do the count. create a dataframe
a <- Corpus(VectorSource(a))
tdm <- TermDocumentMatrix(a, control=list(stopwords=TRUE))
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)
# PDF Output of Wordcloud
pdf('Economic Words.pdf')
wordcloud(d$word, d$freq, max.words=300, random.color=TRUE, min.freq=1, random.order=TRUE)
dev.off()


#### ECON WORD CLOUD
# create a dictionary of all words
econwords=NULL
for (s in HIDict) {
	s <- strsplit(s,"#")[[1]][1]		#split at hash to remove instance ID of same words
	allwords <- c(allwords, strsplit(s," ")[[1]][1])	#returns list, take first element of that array
}
allwords <- tolower(allwords)
allmatch <- match(txtCLEAN, allwords)
#numallmatch <- length(allmatch[which(allmatch>0)])
a <- allwords[allmatch[which(allmatch>0)]]
# convert to Corpus and RDM and do the count. create a dataframe
a <- Corpus(VectorSource(a))
tdm <- TermDocumentMatrix(a, control=list(stopwords=TRUE))
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)
# PDF Output of Wordcloud
pdf('All Words in Dictionary.pdf')
wordcloud(d$word, d$freq, max.words=300, random.color=TRUE, min.freq=1, random.order=TRUE)
dev.off()




### USE tm PACKAGE
# the main data structure is a "corpus" type, a collection of test document
# create a corpus from read text
ctext <- Corpus(VectorSource(txtCLEAN))
#writeCorpus(ctext)		#output txt file

# operate on a single corpus, not loop through each
# eg convert all to lower case to enable word matching consistency
tm_map(ctext, tolower)	#tm_map is a general operator

# remove all punctuation
tm_map(ctext, removePunctuation)

# what is the frequency of word distribution?
# TDM, term document matrix. represent every document as vector of words
# represent each document as a frequency count of an 85,000 length vector(one for each word in the dictionary used)

# create a column for each document, and each row is every word
tdm_text <- TermDocumentMatrix(ctext, control=list(stopwords=TRUE))

# find most frequent terms
findFreqTerms(tdm_text,lowfreq=10)
	
# what about bigrams and trigrams? keep track of order of word
# expand word vector to include phrases
library("wordcloud")
tdm = as.matrix(tdm_text)
wordcount <- sort(rowSums(tdm), decreasing=TRUE)
tdm_names = names(wordcount)
pdf('AllWords no dictionary.pdf')
wordcloud(tdm_names,wordcount, max.words=300)	#pass names to plot and frequency
dev.off()