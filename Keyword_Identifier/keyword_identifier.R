setwd("~/Data Science/Jhon Hopkins/Capstone/Outside Project")

list.files()

library(ggplot2)
library(quanteda)
library(data.table)
library(dplyr)

prueba<-readLines("test.txt",skipNul = TRUE, warn = TRUE)

prueba<-as.data.frame(prueba)
names(prueba)<-c("text")
prueba$text<-as.character(prueba$text)

prueba[which(!complete.cases(prueba)),]<-NULL

length(which(!complete.cases(prueba)))

train.tokens<-tokens(prueba$text,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE,
                     remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

train.tokens<-tokens_tolower(train.tokens)

train.tokens<-tokens_select(train.tokens,stopwords(),selection = "remove")

train.tokens.dfm <- dfm(train.tokens,tolower = FALSE)

unigram_freq<-colSums(train.tokens.dfm)

Unigram <- data.frame(words=names(unigram_freq), count=unigram_freq)

textplot_wordcloud(train.tokens.dfm,min_count = 3, random_order = FALSE,
                                       rotation = .25, 
                                       color = RColorBrewer::brewer.pal(8,"Dark2"))



bigram.token <-tokens_ngrams(train.tokens,n=2,concatenator = " ")

bigram.token.dfm <- dfm(bigram.token,tolower = FALSE)

bigram_freq<-colSums(bigram.token.dfm)

Bigram<-data.frame(word=names(bigram_freq),count=bigram_freq)

Bigram$word<-as.character(Bigram$word)

textplot_wordcloud(bigram.token.dfm,min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))






