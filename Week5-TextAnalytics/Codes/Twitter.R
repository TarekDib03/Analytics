# Week 5 - Twitter


# VIDEO 5; Reading and Pre processing tweeter text data

# Read in the data
setwd("Analytics/Weeks/Week5-TextAnalytics/Data")
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
# Structure of the data frame
str(tweets)


# Create dependent variable
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

#### Preprocessing ### 
# Install and load text mining packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

# Create corpus
corpus = Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus
corpus[[1]]


# Convert to lower-case
corpus = tm_map(corpus, tolower)
corpus[[1]]

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

# Stem document 
corpus = tm_map(corpus, stemDocument)
corpus[[1]]



# Video 6: Preparing text mining data to build the predictive model

# Create matrix
frequencies = DocumentTermMatrix(corpus)
frequencies

# Look at matrix 
inspect(frequencies[1000:1005,505:515])

# Words that appear 100 times in tweets
findFreqTerms(frequencies, lowfreq=100)

# Remove sparse terms. Words that repeat 0.5% or more of the tweets
sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame
tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly (appropriate for R)
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable
tweetsSparse$Negative = tweets$Negative

# Split the data
library(caTools)

set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)



# Video 7: building CART and random forest models

# Build a CART model to predict negative sentiments

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)

# Compute accuracy
(294+18)/(294+6+37+18)

# Baseline accuracy 
table(testSparse$Negative)
300/(300+55)

# Random forest model
library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)

# Logistic model
tweetLog <- glm(Negative ~., data = trainSparse, family="binomial")
predictions <- predict(tweetLog, newdata = testSparse, type = "response")
conf <- table(testSparse$Negative, predictions >= 0.5)
# Accuracy
(conf[1,1]+conf[2,2]) / sum(conf)
# The accuracy which is worse than the baseline. If you were to compute the accuracy on the training 
# set instead, you would see that the model does really well on the training set - this is an example of
# over-fitting. The model fits the training set really well, but does not perform well on the test set. 
# A logistic regression model with a large number of variables is particularly at risk for overfitting.

# The warning messages that you might have seen in this problem have to do with the number of variables,
# and the fact that the model is overfitting to the training se