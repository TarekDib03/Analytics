MITx: 15.071x The Analytics Edge - DETECTING VANDALISM ON WIKIPEDIA
========================================================

## *Introduction*
Wikipedia is a free online encyclopedia that anyone can edit and contribute to. It is available in many languages and is growing all the time. On the English language version of Wikipedia:

    There are currently 4.3 million pages.
    There have been a total of 653 million edits (also called revisions) over its lifetime.
    There are approximately 130,000 edits per day.

One of the consequences of being editable by anyone is that some people vandalize pages. This can take the form of removing content, adding promotional or inappropriate content, or more subtle shifts that change the meaning of the article. With this many articles and edits per day it is difficult for humans to detect all instances of vandalism and revert them. As a result, Wikipedia uses bots - computer programs that automatically revert edits that look like vandalism. In this assignment we will attempt to develop a vandalism detector that uses machine learning to distinguish between a valid edit and vandalism.

The data for this problem is based on the revision history of the page Language. Wikipedia provides a history for each page that consists of the state of the page at each revision. Rather than manually considering each revision, a script was run that checked whether edits stayed or were reverted. If a change was eventually reverted then that revision is marked as vandalism. This may result in some misclassifications, but the script performs well enough for our needs.

As a result of this preprocessing, some common processing tasks have already been done, including lower-casing and punctuation removal. The columns in the dataset are:

    Vandal = 1 if this edit was vandalism, 0 if not.
    Minor = 1 if the user marked this edit as a "minor edit", 0 if not.
    Loggedin = 1 if the user made this edit while using a Wikipedia account, 0 if they were not.
    Added = The unique words added.
    Removed = The unique words removed.

Notice the repeated use of unique. The data we have available is not the bag of words - rather it is the set of words that were removed or added. For example, if a word was removed multiple times in a revision it will only appear one time in the "Removed" column.

## *Bag of Words*

```r
# Data
wiki <- read.csv("wiki.csv", stringsAsFactors = F)

# Convert Vandal column into facto
wiki$Vandal = as.factor(wiki$Vandal)

# How many cases of vandalism were detected in the history of this page?
sum(wiki$Vandal == 1)
```

```
## [1] 1815
```

```r

# Pre process data
library(tm)
# Create Corpus
corpusAdded <- Corpus(VectorSource(wiki$Added))
# Remove Stop words
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
# Look at the first document
corpusAdded[[1]]
```

```
##   repres psycholinguisticspsycholinguist orthographyorthographi help text  action  human ethnologu relationship linguist regard write languag   list xmlspacepreservelanguag metavers formal term philolog common  includ phonologyphonolog often ten list human affili see comput  speechpathologyspeech    way dialect pleas artifici written bodi   quit hypothesi found alon refer   languag profan studi program prioriti rosenfeld technologytechnolog make  first among use languagephilosophi one sound use area creat phrase mark  genet basic famili complet  sapirwhorfhypothesissapirwhorf  talklanguagetalk popul anim  scienc  vocal can concept call   topic locat  number   patholog differ develop 4000 thing idea group complex anim mathemat fair literatur httpwwwzompistcom philosophi  import meaning  historicallinguisticsorphilologyhistor semanticssemant pattern  oral
```

```r
# Create matrix
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
```

```
## A document-term matrix (3876 documents, 6675 terms)
## 
## Non-/sparse entries: 15368/25856932
## Sparsity           : 100%
## Maximal term length: 784 
## Weighting          : term frequency (tf)
```

```r
# Filter out sparse terms by keeping only terms that appear in 0.3% or more
# of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded
```

```
## A document-term matrix (3876 documents, 166 terms)
## 
## Non-/sparse entries: 2681/640735
## Sparsity           : 100%
## Maximal term length: 28 
## Weighting          : term frequency (tf)
```

```r
# Convert sparseAdded to a data frame called wordsAdded, and then prepend
# all the words with the letter A
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))
# Removed Words Process
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
```

```
## A document-term matrix (3876 documents, 5403 terms)
## 
## Non-/sparse entries: 13293/20928735
## Sparsity           : 100%
## Maximal term length: 784 
## Weighting          : term frequency (tf)
```

```r
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
```

```
## A document-term matrix (3876 documents, 162 terms)
## 
## Non-/sparse entries: 2552/625360
## Sparsity           : 100%
## Maximal term length: 28 
## Weighting          : term frequency (tf)
```

```r
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))

# Combine the two dataframes
wikiWords = cbind(wordsAdded, wordsRemoved)
# Add the Vandal variable
wikiWords$Vandal <- wiki$Vandal

# Load CaTools
library(caTools)
set.seed(123)
spl <- sample.split(wikiWords$Vandal, 0.7)
train <- subset(wikiWords, spl == T)
test <- subset(wikiWords, spl == F)
# baseline model accuracy on the test set
table(test$Vandal)[1]/sum(table(test$Vandal))
```

```
##      0 
## 0.5314
```

```r

# CART Model
library(rpart)
library(rpart.plot)
wikiCART <- rpart(Vandal ~ ., data = train, method = "class")
# Predict using the test set
pred = predict(wikiCART, newdata = test, type = "class")
# Accuracy on the test set
t1 <- table(test$Vandal, pred)
(t1[1, 1] + t1[2, 2])/(sum(t1))
```

```
## [1] 0.5417
```

```r
# Plot tree
prp(wikiCART)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


## *PROBLEM-SPECIFIC KNOWLEDGE*
We weren't able to improve on the baseline using the raw textual information. More specifically, the words themselves were not useful. There are other options though, and in this section we will try two techniques - identifying a key class of words, and counting words.

The key class of words we will use are website addresses. "Website addresses" (also known as URLs - Uniform Resource Locators) are comprised of two main parts. An example would be "http://www.google.com". The first part is the protocol, which is usually "http" (HyperText Transfer Protocol). The second part is the address of the site, e.g. "www.google.com". We have stripped all punctuation so links to websites appear in the data as one word, e.g. "httpwwwgooglecom". We hypothesize that given that a lot of vandalism seems to be adding links to promotional or irrelevant websites, the presence of address is a sign of vandalism.

We can search for the presence of a web address in the words added by searching for "http" in the Added column.


```r
# Create a copy of the data frame wiki
wikiWords2 <- wikiWords

# Make a new column in wikiWords2 that is 1 if 'http' was in Added
wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)

# how many revisions added a link?
sum(wikiWords2$HTTP)
```

```
## [1] 217
```

```r

# Training and test data sets
wikiTrain2 = subset(wikiWords2, spl == TRUE)
wikiTest2 = subset(wikiWords2, spl == FALSE)

# Build a CART model
wikiCART2 <- rpart(Vandal ~ ., data = wikiTrain2, method = "class")

# Accuracy
pred2 <- predict(wikiCART2, newdata = wikiTest2, type = "class")
t2 <- table(wikiTest2$Vandal, pred2)
(t2[1, 1] + t2[2, 2])/sum(t2)
```

```
## [1] 0.5727
```

```r

# Sum the rows of dtmAdded and dtmRemoved and add them as new variables in
# data frame wikiWords2
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
# Average number of words added
mean(wikiWords2$NumWordsAdded)
```

```
## [1] 4.05
```

```r

# Build a CART model
wikiTrain3 = subset(wikiWords2, spl == TRUE)
wikiTest3 = subset(wikiWords2, spl == FALSE)
wikiCART3 <- rpart(Vandal ~ ., data = wikiTrain3, method = "class")
pred3 <- predict(wikiCART3, newdata = wikiTest3, type = "class")
t3 <- table(wikiTest3$Vandal, pred3)
(t3[1, 1] + t3[2, 2])/sum(t3)
```

```
## [1] 0.6552
```


## *USING NON-TEXTUAL DATA*


