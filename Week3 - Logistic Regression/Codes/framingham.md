15.071 - Analytics Edge
------------------------------------------------------------------
### The Framingham Heart Study
### By: Tarek Dib
### Date: March 22, 2014

## *Introduction*
The model's main purpose is to predict the 10-year risk of Coronary Heart Disease (CHD). CHD is a disease of the blood vessels supplying the heart. Heart disease has been the leading cause of death worldwide since 1921. In 2008, 7.3 million people died from CHD. The model is built using the following proceedure:
    Identify risk factors
    Collect data
    Predict heart disease
    Collect more data
    Validate model
    Define interventions using model 

# *Independent variables:*
### *Demographic risk factors*
    male: sex of patient
    age: age in years at first examination
    education: Some high school (1), high school/GED (2), some college/vocational school (3), college (4)
### *Behavioral risk factors*
    currentSmoker
    cigsPerDay: Smoking behavior 
### *Medical history risk factors*
    BPmeds: On blood pressure medication at time of first examination
    prevalentStroke: Previously had a stroke
    prevalentHyp: Currently hypertensive
    diabetes: Currently has diabetes 
### *Risk factors from first examination*
    totChol: Total cholesterol (mg/dL)
    sysBP: Systolic blood pressure
    diaBP: Diastolic blood pressure
    BMI: Body Mass Index, weight (kg)/height (m)^2
    heartRate: Heart rate (beats/minute)
    glucose: Blood glucose level (mg/dL)


## *Summary and Classification*

```r
# Read in the dataset setwd('Analytics/Weeks/Week3/Data')
framingham = read.csv("framingham.csv")
```

```
## Warning: cannot open file 'framingham.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r

# Look at structure
str(framingham)
```

```
## Error: object 'framingham' not found
```

```r

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
```

```
## Error: object 'framingham' not found
```

```r

# Split up the data using subset
train = subset(framingham, split == TRUE)
```

```
## Error: object 'framingham' not found
```

```r
test = subset(framingham, split == FALSE)
```

```
## Error: object 'framingham' not found
```


## *The Model, Confusion Matrix and Model Accuracy*

```r
# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
```

```
## Error: object 'train' not found
```

```r
summary(framinghamLog)
```

```
## Error: object 'framinghamLog' not found
```

```r

# Predictions on the test set
predictTest = predict(framinghamLog, type = "response", newdata = test)
```

```
## Error: object 'framinghamLog' not found
```

```r

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)
```

```
## Error: object 'test' not found
```

```r

# Accuracy
(1069 + 11)/(1069 + 6 + 187 + 11)
```

```
## [1] 0.8484
```

```r

# Baseline accuracy
(1069 + 6)/(1069 + 6 + 187 + 11)
```

```
## [1] 0.8445
```


## *Test AUC*

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
ROCRpred = prediction(predictTest, test$TenYearCHD)
```

```
## Error: object 'predictTest' not found
```

```r
as.numeric(performance(ROCRpred, "auc")@y.values)
```

```
## Error: object 'ROCRpred' not found
```

