# Create a Word Cloud
# using code from this example:
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Set directory and environment vaiables

# HOME        <- "/data/anet11.2/tatf/work/uca78"
HOME        <- "c:/R"
PROJECT_DIR <- paste0(HOME,"/Shakespeare")
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

load(file=paste0(DATA_DIR,"/works.RData"))

docs  <- Corpus(VectorSource(works$PlayerLine))
# summary(docs)
# inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("thee", "thou","thy")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

wordcloud(words = docs, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
