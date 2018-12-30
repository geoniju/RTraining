##### Chapter 4: Classification using Naive Bayes --------------------
# Machine Learning with R Book by Brett Lantz
# sudo apt-get install libxml2-dev
install.packages("tm")
library(tm)
library(wordcloud)
library(e1071)
library(gmodels)

## Example: Filtering spam SMS messages ----
## Step 2: Exploring and preparing the data ---- 

# read the sms data into the sms data frame
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

# examine the structure of the sms data
str(sms_raw)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)

sms_corpus <- Corpus(VectorSource(sms_raw$text))

# examine the sms corpus
print(sms_corpus)
inspect(sms_corpus[1:3])

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# examine the clean corpus
inspect(sms_corpus[1:3])
inspect(corpus_clean[1:3])

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# creating training and test datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# word cloud visualization
sms_WordFreq <- sort(colSums(as.matrix(sms_dtm)), decreasing=TRUE)
wordcloud(names(sms_WordFreq), sms_WordFreq, 
          scale=c(3, 0.5),
          min.freq=20, 
          max.words=80, 
          random.order=FALSE, rot.per=.15, colors=brewer.pal(6,"Dark2"))
title(sub="All")

sms_WordFreq <- sort(colSums(as.matrix(sms_dtm[sms_raw$type=="spam",])), decreasing=TRUE)
wordcloud(names(sms_WordFreq), sms_WordFreq, 
          scale=c(3, 0.5),
          min.freq=20, 
          max.words=80, 
          random.order=FALSE, rot.per=.15, colors=brewer.pal(6,"Dark2"))
title(sub="Spam")

sms_WordFreq <- sort(colSums(as.matrix(sms_dtm[sms_raw$type=="ham",])), decreasing=TRUE)
wordcloud(names(sms_WordFreq), sms_WordFreq, 
          scale=c(3, 0.5),
          min.freq=20, 
          max.words=80, 
          random.order=FALSE, rot.per=.15, colors=brewer.pal(6,"Dark2"))
title(sub="Ham")

# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))