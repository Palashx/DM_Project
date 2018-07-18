library(tm)
library(wordcloud)
library(e1071)
library(gmodels)
#library(rplot)
library(rplotengine)
#source("C:\\Users\\Suhail\\Concordia_Masters\\1st_Term\\Data Mining\\PROJECT\\Sentiments\\sampleNaiveBayes\\benchR\\MachineLearningAlg\\learners-in-r\\naiveBayes.R")
set.seed(1234)
rm(list = ls())

## Read Data ----
filesNeg <- list.files("C:\\Users\\jainp\\Documents\\DM\\New folder\\Sample_Proj\\NaiveBayesText\\data\\simp_cornell\\negative\\truthful", pattern="*.txt", full.names=TRUE)
filespos <- list.files("C:\\Users\\jainp\\Documents\\DM\\New folder\\Sample_Proj\\NaiveBayesText\\data\\simp_cornell\\positive\\truthful", pattern="*.txt", full.names=TRUE)

i = 1
df_raw = data.frame(cbind( "sentiment", "text"), stringsAsFactors = FALSE)

names(df_raw) = c("sentiment", "text")
i=1
for (file in filesNeg[1:10]) {
  
  df_raw[i,] = c( "negative", readLines(file))
  i =i+1
}

for (file in filespos[1:10]) {
  df_raw[i,] = c("positive", readLines(file))
  i =i+1
}
df_raw = df_raw[sample(nrow(df_raw)), ]

# Examine the structure of the sms data
str(df_raw)

# Convert spam/ham to factor.
df_raw$sentiment <- factor(df_raw$sentiment)


# Examine the type variable more carefully
str(df_raw$sentiment)
table(df_raw$sentiment)


table(df_raw[, c(1,2)])

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
# examine the clean corpus
inspect(df_corpus[1])
inspect(corpus_clean[1])

# create a document-term sparse matrix
dtm <- DocumentTermMatrix(corpus_clean)
dtm

# creating training and test datasets
train_rows = 1:round(0.7*nrow(df_raw))
test_rows = (round(0.7*nrow(df_raw))+1):nrow(df_raw)
df_raw_train <- df_raw[train_rows, ]
df_raw_test  <- df_raw[test_rows, ]

dtm_train <- dtm[train_rows, ]
dtm_test  <- dtm[test_rows, ]

corpus_train <- corpus_clean[train_rows]
corpus_test  <- corpus_clean[test_rows]

# check that the proportion of spam is similar

prop.table(table(df_raw_train$sentiment))
prop.table(table(df_raw_test$sentiment))

# word cloud visualization
wordFreq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wordcloud(names(wordFreq), wordFreq, 
          scale=c(3, 0.5),
          min.freq=20, 
          max.words=80, 
          random.order=FALSE, rot.per=.15, colors=brewer.pal(6,"Dark2"))
title(sub="All")

wordFreq <- sort(colSums(as.matrix(dtm[df_raw$sentiment=="negative",])), decreasing=TRUE)
wordcloud(names(wordFreq), wordFreq, 
          scale=c(3, 0.5),
          min.freq=20, 
          max.words=80, 
          random.order=FALSE, rot.per=.15, colors=brewer.pal(6,"Dark2"))
title(sub="Deceptive")

wordFreq <- sort(colSums(as.matrix(dtm[df_raw$sentiment=="positive",])), decreasing=TRUE)
wordcloud(names(wordFreq), wordFreq, 
          scale=c(3, 0.5),
          min.freq=20, 
          max.words=80, 
          random.order=FALSE, rot.per=.15, colors=brewer.pal(6,"Dark2"))
title(sub="Truthful")

# indicator features for frequent words
findFreqTerms(dtm_train, 10)
dictionary <- findFreqTerms(dtm_train, 10)
dtm_model_trainset <- DocumentTermMatrix(corpus_train, list(dictionary = dictionary))
dtm_model_testset  <- DocumentTermMatrix(corpus_test, list(dictionary = dictionary))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
dtm_model_trainset <- apply(dtm_model_trainset, MARGIN = 2, convert_counts)
dtm_model_testset  <- apply(dtm_model_testset, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
classifier <- naiveBayes(dtm_model_trainset, df_raw_train$sentiment)
#classifier <- trainNaiveBayes(df_raw_train$sentiment,dtm_model_trainset)
classifier

## Step 4: Evaluating model performance ----
test_pred <- predict(classifier, dtm_model_testset)
#test_pred <-predictNaiveBayes(classifier, dtm_model_testset)
CrossTable(test_pred, df_raw_test$sentiment,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
classifier2 <- naiveBayes(dtm_model_trainset, df_raw_train$sentiment, laplace = 1)
test_pred2 <- predict(classifier2, dtm_model_testset)
CrossTable(test_pred2, df_raw_test$sentiment,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))