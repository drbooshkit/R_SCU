# read website and return html
text <- readLines("http://bahiker.com/eastbayhikes/sibley.html")

# each line is stored as a seperate character string
length(text)

# take substring
#substr(test[400],24,29)

library("tm")
# the tm package includes different readers. readPlain(), readPDF(), readDOC()
# the main data structure is a "corpus" type, a collection of test document

# create a corpus from read text
ctext <- Corpus(VectorSource(text))
writeCorpus(ctext)

# operate on a single corpus, not loop through each
# eg convert all to lower case to enable word matching consistency
tm_map(ctext, tolower)	#tm_map is a general operator

# remove all punctuation
tm_map(ctext, removePunctuation)

# what is the frequency of word distribution?
# TDM, term document matrix. represent every document as vector of words
# represent each document as a frequency count of an 85,000 length vector(one for each word in the dictionary used)

# create a column for each document, and each row is every word
tdm_text <- TermDocumentMatrix(ctext, control=list(minWordLength=1))

# 
#inspect(tdm_text)

# find most frequent terms
findFreqTerms(tdm_text,lowfreq=10)
	
# what about bigrams and trigrams? keep track of order of word
# expand word vector to include phrases

library("wordcloud")
tdm = as.matrix(tdm_text)
wordcount <- sort(rowSums(tdm), decreasing=TRUE)
tdm_names = names(wordcount)
wordcloud(tdm_names,wordcount, max.words=25)	#pass names to plot and frequency

# stemming
#use tm_map