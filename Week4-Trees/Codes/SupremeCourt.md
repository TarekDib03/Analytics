MITx: 15.071x The Analytics Edge - The Steven's Supreme Court Decision
========================================================
# Classification and Regression Tree (CART)
### Tarek Dib
### April 6, 2014

### *Itroduction*
The data are cases from 1994 through 2001. In this period, same nine justices presided Supreme Court of the United States (SCOTUS). They are: Breyer, Ginsburg, Kennedy, O’Connor, Rehnquist (Chief Justice), Scalia, Souter, Stevens, Thomas. This is the longest period of time with the same set of justices in over 180 years. We will focus on predicting Justice Stevens’ decisions, who started out moderate, but became more liberal, self-proclaimmed conservative. 

Note: CART model is a series of decision rules.

### *Variables*
    Dependent Variable: Did Justice Stevens vote to reverse the lower court decision? 1 = reverse, 0 = affirm
    Independent Variables: Properties of the case
    Circuit court of origin (1st – 11th, DC, FED)
    Issue area of case (e.g., civil rights, federal taxation)
    Type of petitioner, type of respondent (e.g., US, an employer)
    Ideological direction of lower court decision (conservative or liberal)
    Whether petitioner argued that a law/practice was unconstitutional
    
    More information about US Court of Appeals can be found in the following link:
    http://en.wikipedia.org/wiki/United_States_courts_of_appeals

### *Building the CART Model* 

```r
# Read in the data
setwd("Analytics/Weeks/Week4/Codes")
```

```
## Error: cannot change working directory
```

```r
stevens = read.csv("stevens.csv")
str(stevens)  # Docket is a unique identifier for each case!
```

```
## 'data.frame':	566 obs. of  9 variables:
##  $ Docket    : Factor w/ 566 levels "00-1011","00-1045",..: 63 69 70 145 97 181 242 289 334 436 ...
##  $ Term      : int  1994 1994 1994 1994 1995 1995 1996 1997 1997 1999 ...
##  $ Circuit   : Factor w/ 13 levels "10th","11th",..: 4 11 7 3 9 11 13 11 12 2 ...
##  $ Issue     : Factor w/ 11 levels "Attorneys","CivilRights",..: 5 5 5 5 9 5 5 5 5 3 ...
##  $ Petitioner: Factor w/ 12 levels "AMERICAN.INDIAN",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ Respondent: Factor w/ 12 levels "AMERICAN.INDIAN",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ LowerCourt: Factor w/ 2 levels "conser","liberal": 2 2 2 1 1 1 1 1 1 1 ...
##  $ Unconst   : int  0 0 0 0 0 1 0 1 0 0 ...
##  $ Reverse   : int  1 1 1 1 1 0 1 1 1 1 ...
```

```r

# Split the data
library(caTools)
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, split == TRUE)
Test = subset(stevens, split == FALSE)

# Install rpart library
install.packages("rpart")
```

```
## Installing package(s) into '/home/tarek/R/i686-pc-linux-gnu-library/2.15'
## (as 'lib' is unspecified)
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(rpart)
install.packages("rpart.plot")
```

```
## Installing package(s) into '/home/tarek/R/i686-pc-linux-gnu-library/2.15'
## (as 'lib' is unspecified)
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
    Unconst, method = "class", data = Train, control = rpart.control(minbucket = 25))

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
tCART <- table(Test$Reverse, PredictCART)
# Accuracy of the model
(tCART[1, 1] + tCART[2, 2])/sum(tCART)
```

```
## [1] 0.6588
```

```r
# Load the ROCR library
library(ROCR)
```

```
## Loading required package: gplots
## Loading required package: gtools
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object(s) are masked from 'package:stats':
## 
##     nobs
## 
## The following object(s) are masked from 'package:utils':
## 
##     object.size
## 
## 
## Attaching package: 'gplots'
## 
## The following object(s) are masked from 'package:stats':
## 
##     lowess
```

```r
# Performance of the model
PredictROC = predict(StevensTree, newdata = Test)
pred = prediction(PredictROC[, 2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")  #tpr: true positive rate. 

# Compute the AUC
as.numeric(performance(pred, "auc")@y.values)
```

```
## [1] 0.6927
```



```r
# Splits
prp(StevensTree)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



```r
# ROC curve
plot(perf)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


### *Random Forests*
This method is similar to the CART method. However, it improves the prediction accuracy of CART and works by building a large number of CART trees! Less interpretable than the CART method.

```r
# Install randomForest package
install.packages("randomForest")
```

```
## Installing package(s) into '/home/tarek/R/i686-pc-linux-gnu-library/2.15'
## (as 'lib' is unspecified)
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object(s) are masked from 'package:gdata':
## 
##     combine
```

```r

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
    LowerCourt + Unconst, data = Train, ntree = 200, nodesize = 25)
```

```
## Warning: The response has five or fewer unique values.  Are you sure you
## want to do regression?
```

```r

set.seed(200)
# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
    LowerCourt + Unconst, data = Train, ntree = 200, nodesize = 25)

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
tRF = table(Test$Reverse, PredictForest)
(tRF[1, 1] + tRF[2, 2])/sum(tRF)
```

```
## [1] 0.6824
```


### *K-fold Cross Validation*
A method to properly select the minibucket parameter in the CART method!
#### *Procedure*
    Giving training set, split into k folds (e.g. k=5)
    Use k-1 folds to estimate a model, and test model on the remaining fold ("Validation set") for each candidate paremeter value. Example select folds 1,2,3,4 and then validate (predict) fold 5. Repeat by selecting folds 1,2,3,5 and validate on fold 4. And so forth. i.e. repeat this for each of the k folds.
    Then for each candidate paremeter and for each fold, compute the accuracy of the model. THen average the accuracy over the k folds.
    

```r
# Install cross-validation packages
install.packages("caret")
```

```
## Installing package(s) into '/home/tarek/R/i686-pc-linux-gnu-library/2.15'
## (as 'lib' is unspecified)
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(caret)
```

```
## Loading required package: cluster
## Loading required package: foreach
## Loading required package: lattice
## Loading required package: plyr
## Loading required package: reshape2
```

```r
install.packages("e1071")
```

```
## Installing package(s) into '/home/tarek/R/i686-pc-linux-gnu-library/2.15'
## (as 'lib' is unspecified)
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(e1071)
```

```
## Loading required package: class
## 
## Attaching package: 'e1071'
## 
## The following object(s) are masked from 'package:gtools':
## 
##     permutations
```

```r

# Define cross-validation experiment
fitControl = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = (1:50) * 0.01)

# Perform the cross validation. This is to compute cp.
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
    data = Train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
```

```
## Warning: executing %dopar% sequentially: no parallel backend registered
```

```
## 396 samples
##   8 predictors
##   2 classes: '0', '1' 
## 
## No pre-processing
## Resampling: Cross-Validation (10 fold) 
## 
## Summary of sample sizes: 357, 356, 357, 357, 356, 357, ... 
## 
## Resampling results across tuning parameters:
## 
##   cp    Accuracy  Kappa  Accuracy SD  Kappa SD
##   0.01  0.6       0.3    0.08         0.2     
##   0.02  0.6       0.2    0.08         0.2     
##   0.03  0.6       0.2    0.09         0.2     
##   0.04  0.6       0.2    0.08         0.2     
##   0.05  0.6       0.2    0.08         0.2     
##   0.06  0.6       0.2    0.08         0.2     
##   0.07  0.6       0.2    0.08         0.2     
##   0.08  0.6       0.2    0.08         0.2     
##   0.09  0.6       0.3    0.1          0.2     
##   0.1   0.6       0.3    0.1          0.2     
##   0.1   0.6       0.3    0.1          0.2     
##   0.1   0.6       0.3    0.1          0.2     
##   0.1   0.6       0.3    0.1          0.2     
##   0.1   0.6       0.3    0.1          0.2     
##   0.2   0.6       0.3    0.1          0.2     
##   0.2   0.6       0.3    0.1          0.2     
##   0.2   0.6       0.3    0.1          0.2     
##   0.2   0.6       0.2    0.06         0.1     
##   0.2   0.6       0.2    0.06         0.1     
##   0.2   0.6       0.2    0.06         0.1     
##   0.2   0.6       0.1    0.05         0.1     
##   0.2   0.6       0.07   0.04         0.1     
##   0.2   0.5       0.02   0.02         0.04    
##   0.2   0.5       0.007  0.01         0.02    
##   0.2   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.3   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.4   0.5       0      0.006        0       
##   0.5   0.5       0      0.006        0       
##   0.5   0.5       0      0.006        0       
##   0.5   0.5       0      0.006        0       
##   0.5   0.5       0      0.006        0       
##   0.5   0.5       0      0.006        0       
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.2.
```

```r

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
    LowerCourt + Unconst, method = "class", data = Train, control = rpart.control(cp = 0.2))

# Make predictions
PredictCV <- predict(StevensTreeCV, newdata = Test, type = "class")
tCV <- table(Test$Reverse, PredictCV)
(tCV[1, 1] + tCV[2, 2])/sum(tCV)
```

```
## [1] 0.7235
```

