options(java.parameters = "-Xmx8000m")
library (NLP)
library (openNLP)
library (openNLPmodels.en)

library (tm)
library (stringr)
library (gsubfn)
library (plyr)
library(proto)
set.seed(1234)
rm(list = ls())

positivePath=("C:\\Users\\jainp\\Documents\\DM\\New folder\\Sample_Proj\\NaiveBayesText\\data\\simp_cornell\\negative\\truthful")

negativePath=("C:\\Users\\jainp\\Documents\\DM\\New folder\\Sample_Proj\\NaiveBayesText\\data\\simp_cornell\\negative\\deceptive")


require ("NLP")
require ("openNLP")
require ("openNLPmodels.en")

tagFreq<- function(pathVar){
  corpus.files = list.files ( path = pathVar,pattern = NULL ,
                              all.files = T ,
                              full.names = T , recursive = T , ignore.case = T ,
                              include.dirs = T)
  corpus.tmp <- lapply (corpus.files[1:100], function ( x ) {
    scan (x , what = " char ", sep = "\t", quiet = T ) } )
  corpus.tmp <- lapply ( corpus.tmp, function ( x ) {
    x <- paste (x , collapse = " ")})
  corpus.tmp <- lapply ( corpus.tmp, function ( x ) {
    x <- enc2utf8 ( x ) } )
  corpus.tmp <- gsub (" {2,}"," ",corpus.tmp )
  corpus.tmp <- str_trim ( corpus.tmp , side = "both")
  Corpus <- lapply(corpus.tmp , function ( x ) {
    x <- as.String(x)})
  
  
  
  Corpus.tagged <-lapply(Corpus,function(x){
    sent_token_annotator<-Maxent_Sent_Token_Annotator()
    word_token_annotator<-Maxent_Word_Token_Annotator()
    pos_tag_annotator<-Maxent_POS_Tag_Annotator()
    y1 <-annotate(x,list(sent_token_annotator,word_token_annotator))
    y2 <-annotate(x,pos_tag_annotator,y1)
    # y3 <- annotate (x, Maxent_POS_Tag_Annotator ( probs = TRUE ),y1)
    y2w<-subset(y2,type=="word")
    tags<-sapply(y2w$features,'[[',"POS")
    r1<-sprintf("%s/%s",x[y2w],tags)
    r2<-paste(r1,collapse=" ")
    return ( r2 ) } )
  
  #extractedTagsFreq<-list(unlist(str_extract_all(unlist(Corpus.tagged), "\\/[A-Z]+")))
  #extractedTagsFreqTable<-table(extractedTagsFreq)
  #extractedTagsFreqDF <-as.data.frame(extractedTagsFreqTable)
  #extractedTagsFreqDFSUB=subset(extractedTagsFreqDF, extractedTagsFreqDF$Freq> 1500)
  #ngramprob<-round(prod(prop.table(extractedTagsFreqDFSUB$Freq)),100)
  
  #probTable <-round(prop.table(testtable),9)
  #return(ngramprob)
  
}

positiveTable<-tagFreq(positivePath)

negativeTable<-tagFreq(negativePath)

