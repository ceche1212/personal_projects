
library(shiny)
library(ggplot2)
library(quanteda)
library(data.table)
library(dplyr)

cleanInput <- function(x){
        x <- tolower(x)
        x <- gsub("\\S*[0-9]+\\S*", " ", x)
        x <- gsub("[^[:alnum:][:space:]'-]", " ", x)
        x <- gsub("(\\w['-]\\w)|[[:punct:]]", "\\1", x)
        x <- gsub("\\s+"," ",x)
        x <- gsub("^\\s+|\\s+$", "", x)
        return(x) 
}

Createunigramtable<-function(x){
        
        
        x<-cleanInput(x)
        x<-as.data.frame(x)
        names(x)<-c("text")
        x$text<-as.character(x$text)
        
        x[which(!complete.cases(x)),]<-NULL
        
        train.tokens<-tokens(x$text,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                             remove_symbols = TRUE, remove_separators = TRUE,
                             remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
        
        train.tokens<-tokens_tolower(train.tokens)
        
        train.tokens<-tokens_select(train.tokens,stopwords(),selection = "remove")
        
        train.tokens<-tokens_select(train.tokens,stopwords('french'),selection = "remove")
        
        train.tokens.dfm <- dfm(train.tokens,tolower = FALSE)
        
        unigram_freq<-colSums(train.tokens.dfm)
        
        Unigram <- data.frame(words=names(unigram_freq), count=unigram_freq)
        
        Unigram<-arrange(Unigram,desc(Unigram$count))
        
        return(head(Unigram,10))
        
        
}

Createbigramtable<-function(x){
        
        x<-cleanInput(x)
        x<-as.data.frame(x)
        names(x)<-c("text")
        x$text<-as.character(x$text)
        
        x[which(!complete.cases(x)),]<-NULL
        
        train.tokens<-tokens(x$text,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                             remove_symbols = TRUE, remove_separators = TRUE,
                             remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
        
        train.tokens<-tokens_tolower(train.tokens)
        
        train.tokens<-tokens_select(train.tokens,stopwords(),selection = "remove")
        
        train.tokens<-tokens_select(train.tokens,stopwords('french'),selection = "remove")
        
        bigram.token <-tokens_ngrams(train.tokens,n=2,concatenator = " ")
        
        bigram.token.dfm <- dfm(bigram.token,tolower = FALSE)
        
        bigram_freq<-colSums(bigram.token.dfm)
        
        Bigram<-data.frame(word=names(bigram_freq),count=bigram_freq)
        
        Bigram<-arrange(Bigram,desc(Bigram$count))
        
        return(head(Bigram,10))
        
        
}

Createtrigramtable<-function(x){
        
        x<-cleanInput(x)
        x<-as.data.frame(x)
        names(x)<-c("text")
        x$text<-as.character(x$text)
        
        x[which(!complete.cases(x)),]<-NULL
        
        train.tokens<-tokens(x$text,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                             remove_symbols = TRUE, remove_separators = TRUE,
                             remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
        
        train.tokens<-tokens_tolower(train.tokens)
        
        train.tokens<-tokens_select(train.tokens,stopwords(),selection = "remove")
        
        train.tokens<-tokens_select(train.tokens,stopwords('french'),selection = "remove")
        
        trigram.token <-tokens_ngrams(train.tokens,n=3,concatenator = " ")
        
        trigram.token.dfm <- dfm(trigram.token,tolower = FALSE)
        
        trigram_freq<-colSums(trigram.token.dfm)
        
        Trigram<-data.frame(word=names(trigram_freq),count=trigram_freq)
        
        Trigram<-arrange(Trigram,desc(Trigram$count))
        
        return(head(Trigram,5))
        
        
}

Createcloudwordunigram<-function(x){
        
        x<-cleanInput(x)
        x<-as.data.frame(x)
        names(x)<-c("text")
        x$text<-as.character(x$text)
        
        x[which(!complete.cases(x)),]<-NULL
        
        train.tokens<-tokens(x$text,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                             remove_symbols = TRUE, remove_separators = TRUE,
                             remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
        
        train.tokens<-tokens_tolower(train.tokens)
        
        train.tokens<-tokens_select(train.tokens,stopwords(),selection = "remove")
        
        train.tokens<-tokens_select(train.tokens,stopwords('french'),selection = "remove")
        
        train.tokens.dfm <- dfm(train.tokens,tolower = FALSE)
        
        wc1<-textplot_wordcloud(train.tokens.dfm,min_count = 2, random_order = FALSE,
                                rotation = .25, 
                                color = RColorBrewer::brewer.pal(8,"Dark2"),min_size = 2, max_size = 6)
        
        return(wc1)
        
        
}

Createcloudwordbigram<-function(x){
        
        x<-cleanInput(x)
        x<-as.data.frame(x)
        names(x)<-c("text")
        x$text<-as.character(x$text)
        
        x[which(!complete.cases(x)),]<-NULL
        
        train.tokens<-tokens(x$text,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                             remove_symbols = TRUE, remove_separators = TRUE,
                             remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
        
        train.tokens<-tokens_tolower(train.tokens)
        
        train.tokens<-tokens_select(train.tokens,stopwords(),selection = "remove")
        
        train.tokens<-tokens_select(train.tokens,stopwords('french'),selection = "remove")
        
        bigram.token <-tokens_ngrams(train.tokens,n=2,concatenator = " ")
        
        bigram.token.dfm <- dfm(bigram.token,tolower = FALSE)
        
        wc2<-textplot_wordcloud(bigram.token.dfm,min_count = 2, random_order = FALSE,
                           rotation = .25, 
                           color = RColorBrewer::brewer.pal(8,"Dark2"),min_size = 2, max_size = 6)
        
        return(wc2)
        
        
}

printoriginal<-function(x){
        
        
        print(x)
}


shinyServer(function(input, output) {
   
        output$prueba3 <- renderTable({Createunigramtable(input$inputText)
                
        })
        
        
        output$prueba2 <- renderTable({Createbigramtable(input$inputText)
                
        })
        
        output$prueba4 <- renderTable({Createtrigramtable(input$inputText)
                
        })
        
        output$wordcloud1 <- renderPlot({Createcloudwordunigram(input$inputText)
                
        })
        
        output$wordcloud2 <- renderPlot({Createcloudwordbigram(input$inputText)
                
        })
        
        output$original <- renderText({printoriginal(input$inputText)
                
        })
  
  
  
})
