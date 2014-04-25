15.071 - Analytics Edge
------------------------------------------------------------------
#### The Framingham Heart Study
#### Tarek Dib
#### Date: March 23, 2014

## *Introduction*
The model's main purpose is to predict the 10-year risk of Coronary Heart Disease (CHD). CHD is a disease of the blood vessels supplying the heart. Heart disease has been the leading cause of death worldwide since 1921. In 2008, 7.3 million people died from CHD. The model is built using the following proceedure:
    Identify risk factors
    Collect data
    Predict heart disease
    Collect more data
    Validate model
    Define interventions using model 

### *Independent variables:*
#### *Demographic risk factors*
    male: sex of patient
    age: age in years at first examination
    education: Some high school (1), high school/GED (2), some college/vocational school (3), college (4)
#### *Behavioral risk factors*
    currentSmoker
    cigsPerDay: Smoking behavior 
#### *Medical history risk factors*
    BPmeds: On blood pressure medication at time of first examination
    prevalentStroke: Previously had a stroke
    prevalentHyp: Currently hypertensive
    diabetes: Currently has diabetes 
#### *Risk factors from first examination*
    totChol: Total cholesterol (mg/dL)
    sysBP: Systolic blood pressure
    diaBP: Diastolic blood pressure
    BMI: Body Mass Index, weight (kg)/height (m)^2
    heartRate: Heart rate (beats/minute)
    glucose: Blood glucose level (mg/dL)
More information about the data set used in the study can be found at: https://biolincc.nhlbi.nih.gov/static/studies/teaching/framdoc.pdf
## *Summary and Classification*

```r
# Read in the dataset
setwd("Analytics/Weeks/Week3/Data")
```

```
## Error: cannot change working directory
```

```r
framingham <- read.csv("framingham.csv")

# Structure of data
str(framingham)
```

```
## 'data.frame':	4240 obs. of  16 variables:
##  $ male           : int  1 0 1 0 0 0 0 0 1 1 ...
##  $ age            : int  39 46 48 61 46 43 63 45 52 43 ...
##  $ education      : int  4 2 1 3 3 2 1 2 1 1 ...
##  $ currentSmoker  : int  0 0 1 1 1 0 0 1 0 1 ...
##  $ cigsPerDay     : int  0 0 20 30 23 0 0 20 0 30 ...
##  $ BPMeds         : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ prevalentStroke: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ prevalentHyp   : int  0 0 0 1 0 1 0 0 1 1 ...
##  $ diabetes       : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ totChol        : int  195 250 245 225 285 228 205 313 260 225 ...
##  $ sysBP          : num  106 121 128 150 130 ...
##  $ diaBP          : num  70 81 80 95 84 110 71 71 89 107 ...
##  $ BMI            : num  27 28.7 25.3 28.6 23.1 ...
##  $ heartRate      : int  80 95 75 65 85 77 60 79 76 93 ...
##  $ glucose        : int  77 76 70 103 85 99 85 78 79 88 ...
##  $ TenYearCHD     : int  0 0 0 1 0 0 1 0 0 0 ...
```

```r

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
# One needs to put between 50% and 80% of data in the training set
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
```


## *The Model, Confusion Matrix and Model Accuracy*

```r
# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)
```

```
## 
## Call:
## glm(formula = TenYearCHD ~ ., family = binomial, data = train)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.849  -0.601  -0.426  -0.284   2.837  
## 
## Coefficients:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -7.88657    0.89073   -8.85  < 2e-16 ***
## male             0.52846    0.13544    3.90  9.6e-05 ***
## age              0.06206    0.00834    7.44  1.0e-13 ***
## education       -0.05892    0.06243   -0.94   0.3453    
## currentSmoker    0.09324    0.19401    0.48   0.6308    
## cigsPerDay       0.01501    0.00783    1.92   0.0551 .  
## BPMeds           0.31122    0.28741    1.08   0.2789    
## prevalentStroke  1.16579    0.57121    2.04   0.0413 *  
## prevalentHyp     0.31582    0.17176    1.84   0.0660 .  
## diabetes        -0.42149    0.40799   -1.03   0.3016    
## totChol          0.00384    0.00138    2.79   0.0053 ** 
## sysBP            0.01134    0.00457    2.48   0.0130 *  
## diaBP           -0.00474    0.00800   -0.59   0.5535    
## BMI              0.01072    0.01616    0.66   0.5069    
## heartRate       -0.00810    0.00531   -1.52   0.1274    
## glucose          0.00893    0.00284    3.15   0.0016 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2020.7  on 2384  degrees of freedom
## Residual deviance: 1792.3  on 2369  degrees of freedom
##   (371 observations deleted due to missingness)
## AIC: 1824
## 
## Number of Fisher Scoring iterations: 5
```

```r

# Predictions on the test set
predictTest = predict(framinghamLog, type = "response", newdata = test)

# Confusion matrix with threshold of 0.5
ConfMat <- table(test$TenYearCHD, predictTest > 0.5)
ConfMat
```

```
##    
##     FALSE TRUE
##   0  1069    6
##   1   187   11
```

```r

# Accuracy
(ConfMat[1, 1] + ConfMat[2, 2])/(ConfMat[1, 1] + ConfMat[1, 2] + ConfMat[2, 
    1] + ConfMat[2, 2])
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
as.numeric(performance(ROCRpred, "auc")@y.values)
```

```
## [1] 0.7421
```

