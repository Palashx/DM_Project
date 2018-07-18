library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(RWeka)

set.seed(1234)
rm(list = ls())

filesNeg <- list.files("C:\\Users\\jainp\\Documents\\DM\\New folder\\Sample_Proj\\NaiveBayesText\\data\\simp_cornell\\negative", pattern="*.txt", full.names=TRUE)
filespos <- list.files("C:\\Users\\jainp\\Documents\\DM\\New folder\\Sample_Proj\\NaiveBayesText\\data\\simp_cornell\\positive", pattern="*.txt", full.names=TRUE)


positivePath=("C:\\Users\\jainp\\Documents\\DM\\New folder\\Sample_Proj\\NaiveBayesText\\data\\simp_cornell\\negative")

negativePath=("C:\\Users\\jainp\\Documents\\DM\\New folder\\Sample_Proj\\NaiveBayesText\\data\\simp_cornell\\positive")

#bigramFreq<-function(pathVar,sentiment){
  corpus.files = list.files ( path = positivePath,pattern = NULL ,
                              all.files = T ,
                              full.names = T , recursive = T , ignore.case = T ,
                              include.dirs = T)
  
  
  df_raw = data.frame(cbind( "sentiment", "text"), stringsAsFactors = FALSE)
  
  names(df_raw) = c("sentiment", "text")
  i=1
  for (file in corpus.files[1:100]) {
    
    df_raw[i,] = c( "positive", readLines(file))
    i =i+1
  }
  
  df_corpus <- Corpus(VectorSource(df_raw$text))
  
  # examine the sms corpus
  print(df_corpus)
  inspect(df_corpus[1:2])
  
  # clean up the corpus using tm_map()
  corpus_clean <- tm_map(df_corpus, content_transformer(tolower))
  corpus_clean <- tm_map(corpus_clean, removeNumbers)
  corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
  corpus_clean <- tm_map(corpus_clean, removePunctuation)
  corpus_clean <- tm_map(corpus_clean, stripWhitespace)
  print(corpus_clean)
  inspect(corpus_clean)
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tdm.bigram = TermDocumentMatrix(corpus_clean,
                                  control = list(tokenize = BigramTokenizer))
  
  freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  freq.dfSUB=subset(freq.df, freq.df$freq> 100)
  ngramprob<-round(prod(prop.table(freq.dfSUB$freq)),100)
  
#   return(test)
  
#}

#freqPOsiv<-bigramFreq(positivePath,"positive")





