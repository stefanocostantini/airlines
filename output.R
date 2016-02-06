library(tm)
library(wordcloud)

load("lda_model.RData")
load("results.RData")



wordcloud(colnames(fit$topics),fit$topics[1,])
