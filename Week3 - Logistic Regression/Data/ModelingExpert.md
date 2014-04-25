15.071 - Analytics Edge
------------------------------------------------------------------
#### Claims Data
#### Tarek Dib
#### Date: March 22, 2014

### *Introduction*
Data of 131 Diabetes patients were selected randomly from the D2Hawkeye database. The patients range in age from 35 to 55 years. The costs range from $10,000 to $20,000. The dates of these claims range from September 1, 2003 to August 31, 2005.

Expert physician reviewed claims and wrote descriptive notes, and rated quality on a two-point
scale (poor/good). Dependent variable is quality of care. Independent variables are listed below in addidition to the binary dependent variable PoorCare (0 if the patient had good care and 1 if he/she had poor care).

The variables in the dataset quality.csv are as follows:

    MemberID numbers the patients from 1 to 131, and is just an identifying number.
    InpatientDays is the number of inpatient visits, or number of days the person spent in the hospital.
    ERVisits is the number of times the patient visited the emergency room.
    OfficeVisits is the number of times the patient visited any doctor's office.
    Narcotics is the number of prescriptions the patient had for narcotics.
    DaysSinceLastERVisit is the number of days between the patient's last emergency room visit and the end of the study period (set to the length of the study period if they never visited the ER). 
    Pain is the number of visits for which the patient complained about pain.
    TotalVisits is the total number of times the patient visited any healthcare provider.
    ProviderCount is the number of providers that served the patient.
    MedicalClaims is the number of days on which the patient had a medical claim.
    ClaimLines is the total number of medical claims.
    StartedOnCombination is whether or not the patient was started on a combination of drugs to treat their diabetes (TRUE or FALSE).
    AcuteDrugGapSmall is the fraction of acute drugs that were refilled quickly after the prescription ran out.
    PoorCare is the outcome or dependent variable, and is equal to 1 if the patient had poor care, and equal to 0 if the patient had good care.

## *Summary and Classification*

```r
quality = read.csv("quality.csv")
# Look at structure
str(quality)
```

```
## 'data.frame':	131 obs. of  14 variables:
##  $ MemberID            : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ InpatientDays       : int  0 1 0 0 8 2 16 2 2 4 ...
##  $ ERVisits            : int  0 1 0 1 2 0 1 0 1 2 ...
##  $ OfficeVisits        : int  18 6 5 19 19 9 8 8 4 0 ...
##  $ Narcotics           : int  1 1 3 0 3 2 1 0 3 2 ...
##  $ DaysSinceLastERVisit: num  731 411 731 158 449 ...
##  $ Pain                : int  10 0 10 34 10 6 4 5 5 2 ...
##  $ TotalVisits         : int  18 8 5 20 29 11 25 10 7 6 ...
##  $ ProviderCount       : int  21 27 16 14 24 40 19 11 28 21 ...
##  $ MedicalClaims       : int  93 19 27 59 51 53 40 28 20 17 ...
##  $ ClaimLines          : int  222 115 148 242 204 156 261 87 98 66 ...
##  $ StartedOnCombination: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ AcuteDrugGapSmall   : int  0 1 5 0 0 4 0 0 0 0 ...
##  $ PoorCare            : int  0 0 0 0 0 1 0 0 1 0 ...
```

```r

# Table outcome
table(quality$PoorCare)
```

```
## 
##  0  1 
## 98 33
```

```r
# Percentage of patients who receive good quality care
table(quality$PoorCare)[1]/nrow(quality)  # ~75%
```

```
##      0 
## 0.7481
```

```r

# Load the classification library caTools
library(caTools)

# Split the data into training and test sets
set.seed(888)
spl = sample(1:nrow(quality), size = 0.7 * nrow(quality))
train = quality[spl, ]
test = quality[-spl, ]

# Randomly split data
set.seed(88)
# 75% of patients in the training set are receiving good care, and 75% of
# patients in the test set are receing good care
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split  # True means that we should put that observation in the training set. False means we should put it in the test set.
```

```
##   [1]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE
##  [12] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [23]  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
##  [34]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE
##  [45] FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE
##  [56]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
##  [67]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [78]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
##  [89]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
## [100]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
## [111] FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE
## [122]  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE
```

```r
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
```


## *Building the Model*

```r
# Logistic Regression Model
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, 
    family = binomial)
summary(QualityLog)
```

```
## 
## Call:
## glm(formula = PoorCare ~ StartedOnCombination + ProviderCount, 
##     family = binomial, data = qualityTrain)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.6183  -0.7278  -0.6455  -0.0841   1.9466  
## 
## Coefficients:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -2.0010     0.5510   -3.63  0.00028 ***
## StartedOnCombinationTRUE   1.9523     1.2234    1.60  0.11054    
## ProviderCount              0.0337     0.0198    1.70  0.08971 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.89  on 98  degrees of freedom
## Residual deviance: 104.37  on 96  degrees of freedom
## AIC: 110.4
## 
## Number of Fisher Scoring iterations: 4
```

```r
# All else being equal, the above model implies that starting a patient on
# a combination of drugs is indicative of poor care because the
# coefficient value is positive, meaning that positive values of the
# variable make the outcome of 1 more likely.

# Make predictions on training set
predictTrain = predict(QualityLog, type = "response")

# Analyze predictions
summary(predictTrain)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.138   0.183   0.215   0.253   0.284   0.932
```

```r
tapply(predictTrain, qualityTrain$PoorCare, mean)
```

```
##      0      1 
## 0.2308 0.3167
```


## *Confusion Matrix*

```r
# Confusion matrix for threshold of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)
```

```
##    
##     FALSE TRUE
##   0    73    1
##   1    22    3
```

```r

# Sensitivity and Specificity
Sensitivity <- table(qualityTrain$PoorCare, predictTrain > 0.5)[2, 2]/(table(qualityTrain$PoorCare, 
    predictTrain > 0.5)[2, 1] + table(qualityTrain$PoorCare, predictTrain > 
    0.5)[2, 2])
Specificity <- table(qualityTrain$PoorCare, predictTrain > 0.5)[1, 1]/(table(qualityTrain$PoorCare, 
    predictTrain > 0.5)[1, 1] + table(qualityTrain$PoorCare, predictTrain > 
    0.5)[1, 2])
Sensitivity
```

```
## [1] 0.12
```

```r
Specificity
```

```
## [1] 0.9865
```

```r

# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)
```

```
##    
##     FALSE TRUE
##   0    73    1
##   1    23    2
```


## *Receiver Operating Characteristic Curve (ROC Curve)*

```r
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

# Prediction function
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
```


## *ROC Curve*

```r
# ROC Curve with threshold labels and added colors
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.2), text.adj = c(-0.2, 
    1.5))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


## *Predicting using the test set*

```r
predictTest = predict(QualityLog, type = "response", newdata = qualityTest)
# compute the test set AUC
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
```

```
## [1] 0.763
```

The AUC of a model has the following nice interpretation: given a random patient who actually received poor care, and a random patient who actually received good care, the AUC is the perecentage of time that our model will classify which is which correctly. 
