Analytics Edge, MITx: 15.07x, Modeling Expert
------------------------------------------------------------------
### By: Tarek Dib
### Date: March 22, 2014



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

setwd("Analytics/Weeks/Week3/Data")

## *Summary and Classification*

```r

setwd("Analytics/Weeks/Week3/Data")
```

```
## Error: cannot change working directory
```

```r
quality = read.csv("quality.csv")
```

```
## Warning: cannot open file 'quality.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
# Look at structure
str(quality)
```

```
## Error: object 'quality' not found
```

```r

# Table outcome
table(quality$PoorCare)
```

```
## Error: object 'quality' not found
```

```r
# Percentage of patients who receive good quality care
table(quality$PoorCare)[1]/nrow(quality)  # ~75%
```

```
## Error: object 'quality' not found
```

```r

# Load the classification library caTools
library(caTools)

# Split the data into training and test sets
set.seed(888)
spl = sample(1:nrow(data), size = 0.7 * nrow(data))
```

```
## Error: argument of length 0
```

```r
train = data[spl, ]
```

```
## Error: object 'spl' not found
```

```r
test = data[-spl, ]
```

```
## Error: object 'spl' not found
```

```r

# Randomly split data
set.seed(88)
# 75% of patients in the training set are receiving good care, and 75% of
# patients in the test set are receing good care
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
```

```
## Error: object 'quality' not found
```

```r
split  # True means that we should put that observation in the training set. False means we should put it in the test set.
```

```
## function (x, f, drop = FALSE, ...) 
## UseMethod("split")
## <bytecode: 0x9f233fc>
## <environment: namespace:base>
```

```r
qualityTrain = subset(quality, split == TRUE)
```

```
## Error: object 'quality' not found
```

```r
qualityTest = subset(quality, split == FALSE)
```

```
## Error: object 'quality' not found
```


## *Building the Model*

```r
# Logistic Regression Model
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, 
    family = binomial)
```

```
## Error: object 'qualityTrain' not found
```

```r
summary(QualityLog)
```

```
## Error: object 'QualityLog' not found
```

```r
# All else being equal, the above model implies that starting a patient on
# a combination of drugs is indicative of poor care because the
# coefficient value is positive, meaning that positive values of the
# variable make the outcome of 1 more likely.

# Make predictions on training set
predictTrain = predict(QualityLog, type = "response")
```

```
## Error: object 'QualityLog' not found
```

```r

# Analyze predictions
summary(predictTrain)
```

```
## Error: object 'predictTrain' not found
```

```r
tapply(predictTrain, qualityTrain$PoorCare, mean)
```

```
## Error: object 'qualityTrain' not found
```


## *Confusion Matrix*

```r
# Confusion matrix for threshold of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)
```

```
## Error: object 'qualityTrain' not found
```

```r

# Sensitivity and Specificity
Sensitivity <- table(qualityTrain$PoorCare, predictTrain > 0.5)[2, 2]/(table(qualityTrain$PoorCare, 
    predictTrain > 0.5)[2, 1] + table(qualityTrain$PoorCare, predictTrain > 
    0.5)[2, 2])
```

```
## Error: object 'qualityTrain' not found
```

```r
Specificity <- table(qualityTrain$PoorCare, predictTrain > 0.5)[1, 1]/(table(qualityTrain$PoorCare, 
    predictTrain > 0.5)[1, 1] + table(qualityTrain$PoorCare, predictTrain > 
    0.5)[1, 2])
```

```
## Error: object 'qualityTrain' not found
```

```r
Sensitivity
```

```
## Error: object 'Sensitivity' not found
```

```r
Specificity
```

```
## Error: object 'Specificity' not found
```

```r

# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)
```

```
## Error: object 'qualityTrain' not found
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
```

```
## Error: object 'predictTrain' not found
```

```r

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
```

```
## Error: object 'ROCRpred' not found
```


## *ROC Curve*

```r
# ROC Curve with threshold labels and added colors
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 
    1.7))
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'plot': Error: object 'ROCRperf' not found
```


