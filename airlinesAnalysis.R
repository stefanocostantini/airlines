# Load required Twitter libraries
library(twitteR)
library(tm)
## install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library(tm.lexicon.GeneralInquirer) # dictionary of positive and negative terms
library(wordcloud)
library(streamR)

# Load data (set correct working directory)
load("ryanair.RData")
load("easyJet.RData")
load("vueling.RData")
load("norwegian.RData")
load("ba.RData")

# Generate dataframes from raw data

easyjet.df <- as.data.frame(easyJet[[1]])
ryanair.df <- as.data.frame(ryanair[[1]])
vueling.df <- as.data.frame(vueling[[1]])
norwegian.df <- as.data.frame(norwegian[[1]])
ba.df <- as.data.frame(ba[[1]])

for (i in 1:length(easyJet)){
  easyjet.df[i,] <- as.data.frame(easyJet[[i]])
}

for (i in 1:length(ryanair)){
  ryanair.df[i,] <- as.data.frame(ryanair[[i]])
}

for (i in 1:length(vueling)){
  vueling.df[i,] <- as.data.frame(vueling[[i]])
}

for (i in 1:length(norwegian)){
  norwegian.df[i,] <- as.data.frame(norwegian[[i]])
}

for (i in 1:length(ba)){
  ba.df[i,] <- as.data.frame(ba[[i]])
}


tweets.df <- rbind(easyjet.df,ryanair.df,vueling.df,norwegian.df,ba.df)
airlines <- c(rep("easyjet",nrow(easyjet.df)),
              rep("ryanair",nrow(ryanair.df)),
              rep("vueling",nrow(vueling.df)),
              rep("norwegian",nrow(norwegian.df)),
              rep("ba",nrow(ba.df)))
tweets.df$airline <- airlines

# Save dataframe as RData and CSV
# save(tweets.df,file="tweets.RData")
# write.csv(tweets.df,file="tweets.csv",row.names = FALSE)

# Text analysis
# =============

# Load data
load("tweets.RData")

# Create a smaller dataframe for analysis
tweetsText <- tweets.df[,c(1,3,12,17)]

# fix encoding on mac
tweetsText[,1] <- iconv(tweetsText[,1],to="utf-8-mac")
# remove retweet entities
tweetsText[,1] <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetsText[,1])
# remove at people
tweetsText[,1] <- gsub("@\\w+", "", tweetsText[,1])
# remove tags
tweetsText[,1] <- gsub("#"," ",tweetsText[,1])
# remove punctuation
tweetsText[,1] <- gsub("[[:punct:]]", "", tweetsText[,1])
# remove numbers
tweetsText[,1] <- gsub("[[:digit:]]", "", tweetsText[,1])
# remove html links
tweetsText[,1] <- gsub("http\\w+", "", tweetsText[,1])
# force to lowercase
tweetsText[,1] <- tolower(tweetsText[,1])
# remove stopwords
tweetsText[,1] <- removeWords(tweetsText[,1], stopwords("SMART"))
tweetsText[,1] <- removeWords(tweetsText[,1], c("i","hi",
                                                "easyJet","easyjet",
                                                "ryanair","Ryanair",
                                                "vueling","Vueling",
                                                "British Airways","BA",
                                                "Norwegian","flight"))
# stemming
tweetsText[,1] <- stemDocument(tweetsText[,1], language = "english")
# remove unnecessary spaces
tweetsText[,1] <- gsub("[ \t]{2,}", " ", tweetsText[,1])
tweetsText[,1] <- gsub("^\\s+|\\s+$", "", tweetsText[,1])

# Topic modelling
# (code inspired by: http://cpsievert.github.io/LDAvis/reviews/reviews.html)
tweetList <- strsplit(tweetsText[,1], "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(tweetList))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(tweetList, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data 
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 

# MCMC and model tuning parameters:
K <- 50
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(123)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 1000,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2-t1

# Save fitted model
save(fit,file="lda_model.RData")

# Turn topics into clouds for analysis
for (j in 1:K){
  png(paste("clouds/",j,".png",sep=""))
  wordcloud(vocab, fit$topics[j,], scale=c(3,.5), min.freq = 2, max.words = 100)
  dev.off()
}

# For better wordclouds of topics
# http://www.r-bloggers.com/word-cloud-in-r/

# Load fitted model
load("lda_model.RData")

# Do sentiment analysis on chosen topics
#
# Convert each topic into a vector of words based on their
# distribution

topics <- as.data.frame(matrix(c(0,0),ncol=2))
colnames(topics) <- c("topic","content")

for (i in 1:K){
  cat("Topic ",i,"\n")
  topic <- c()
  for (w in 1:length(vocab)){
    topic <- c(topic,rep(vocab[w],fit$topics[i,w]))
  }
  topic.row <- paste(topic,collapse=" ")
  topics[i,1] <- i
  topics[i,2] <- topic.row
}

# Create a corpus for the analysis

topics.corpus <- Corpus(VectorSource(topics[,2]))

# Assess the polarity of each topic

pos <- sapply(topics.corpus, tm_term_score, 
              terms_in_General_Inquirer_categories("Positiv"))
neg <- sapply(topics.corpus, tm_term_score, 
              terms_in_General_Inquirer_categories("Negativ"))

raw.scores <- pos-neg 

# Normalise score by the number of word frequencies
scores <- raw.scores / rowSums(fit$topics)

# Give a scoring to each airline based on the allocation of tweets

