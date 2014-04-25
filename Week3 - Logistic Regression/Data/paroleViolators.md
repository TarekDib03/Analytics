MITx: 15.071x The Analytics Edge
------------------------------------------------------------------
# Predicting parole violators
#### Tarek Dib
#### Date: March 24, 2014

### *Introduction*

In many criminal justice systems around the world, inmates deemed not to be a threat to society are released from prison under the parole system prior to completing their sentence. They are still considered to be serving their sentence while on parole, and they can be returned to prison if they violate the terms of their parole.

Parole boards are charged with identifying which inmates are good candidates for release on parole. They seek to release inmates who will not commit additional crimes after release. In this problem, we will build and validate a model that predicts if an inmate will violate the terms of his or her parole. Such a model could be useful to a parole board when deciding to approve or deny an application for parole.

For this prediction task, we will use data from the United States 2004 National Corrections Reporting Program, a nationwide census of parole releases that occurred during 2004. We limited our focus to parolees who served no more than 6 months in prison and whose maximum sentence for all charges did not exceed 18 months. The dataset contains all such parolees who either successfully completed the term of parole during 2004 or those who violated the terms of their parole during that year. The dataset contains the following variables:

    male: 1 if the parolee is male, 0 if female
    race: 1 if the parolee is white, 2 otherwise
    age: the parolee's age in years at release from prison
    state: a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. The three states were selected due to having a high representation in the dataset.
    time.served: the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
    max.sentence: the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
    multiple.offenses: 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
    crime: a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
    violator: 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.

## *Loading and Preparing the Dataset*

```r
# Read the data set
parole <- read.csv("parole.csv")

# Structure and summary
str(parole)
```

```
## 'data.frame':	675 obs. of  9 variables:
##  $ male             : int  1 0 1 1 1 1 1 0 0 1 ...
##  $ race             : int  1 1 2 1 2 2 1 1 1 2 ...
##  $ age              : num  33.2 39.7 29.5 22.4 21.6 46.7 31 24.6 32.6 29.1 ...
##  $ state            : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ time.served      : num  5.5 5.4 5.6 5.7 5.4 6 6 4.8 4.5 4.7 ...
##  $ max.sentence     : int  18 12 12 18 12 18 18 12 13 12 ...
##  $ multiple.offenses: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ crime            : int  4 3 3 1 1 4 3 1 3 2 ...
##  $ violator         : int  0 0 0 0 0 0 0 0 0 0 ...
```

```r
summary(parole)
```

```
##       male            race           age           state     
##  Min.   :0.000   Min.   :1.00   Min.   :18.4   Min.   :1.00  
##  1st Qu.:1.000   1st Qu.:1.00   1st Qu.:25.4   1st Qu.:2.00  
##  Median :1.000   Median :1.00   Median :33.7   Median :3.00  
##  Mean   :0.807   Mean   :1.42   Mean   :34.5   Mean   :2.89  
##  3rd Qu.:1.000   3rd Qu.:2.00   3rd Qu.:42.5   3rd Qu.:4.00  
##  Max.   :1.000   Max.   :2.00   Max.   :67.0   Max.   :4.00  
##   time.served    max.sentence  multiple.offenses     crime     
##  Min.   :0.00   Min.   : 1.0   Min.   :0.000     Min.   :1.00  
##  1st Qu.:3.25   1st Qu.:12.0   1st Qu.:0.000     1st Qu.:1.00  
##  Median :4.40   Median :12.0   Median :1.000     Median :2.00  
##  Mean   :4.20   Mean   :13.1   Mean   :0.536     Mean   :2.06  
##  3rd Qu.:5.20   3rd Qu.:15.0   3rd Qu.:1.000     3rd Qu.:3.00  
##  Max.   :6.00   Max.   :18.0   Max.   :1.000     Max.   :4.00  
##     violator    
##  Min.   :0.000  
##  1st Qu.:0.000  
##  Median :0.000  
##  Mean   :0.116  
##  3rd Qu.:0.000  
##  Max.   :1.000
```

```r

# Number of violators
table(parole$violator)[2]
```

```
##  1 
## 78
```

```r

# Convert to factors
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
```


## *Training and Testing Datasets*

```r
# Splitting the data sets into training and test sets
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
```


## *Building the Logistic Regression Model*

```r
mod <- glm(violator ~ ., data = train, family = "binomial")
summary(mod)
```

```
## 
## Call:
## glm(formula = violator ~ ., family = "binomial", data = train)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.704  -0.424  -0.272  -0.169   2.837  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -4.241157   1.293885   -3.28    0.001 ** 
## male               0.386990   0.437961    0.88    0.377    
## race               0.886719   0.395066    2.24    0.025 *  
## age               -0.000176   0.016085   -0.01    0.991    
## state2             0.443301   0.481662    0.92    0.357    
## state3             0.834980   0.556270    1.50    0.133    
## state4            -3.396788   0.611586   -5.55  2.8e-08 ***
## time.served       -0.123887   0.120423   -1.03    0.304    
## max.sentence       0.080295   0.055375    1.45    0.147    
## multiple.offenses  1.611992   0.385305    4.18  2.9e-05 ***
## crime2             0.683714   0.500355    1.37    0.172    
## crime3            -0.278105   0.432836   -0.64    0.521    
## crime4            -0.011763   0.571304   -0.02    0.984    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 340.04  on 472  degrees of freedom
## Residual deviance: 251.48  on 460  degrees of freedom
## AIC: 277.5
## 
## Number of Fisher Scoring iterations: 6
```

```r

# Significant predictors
id <- which(summary(mod)$coeff[, 4] < 0.05)
# Cofficients of the significant predictors
coeff.sig <- summary(mod)$coeff[, 1][id]

# A parolee who is male, of white race, aged 50 years at prison release,
# from the state of Maryland, served 3 months, had a maximum sentence of
# 12 months, did not commit multiple offenses, and committed a larceny.
# Obtain odds and probability that he is a violator. From the logistic
# regression equation, we have log(odds) = -4.2411574 + 0.3869904*male +
# 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 -
# 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence +
# 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 -
# 0.0117627*crime4. This parolee has male=1, race=1, age=50, state2=0,
# state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0,
# crime2=1, crime3=0, crime4=0. We conclude that log(odds) = -1.700629.
odds <- as.numeric(exp(coefficients(mod)[c("(Intercept)")] + coefficients(mod)[c("male")] + 
    coefficients(mod)[c("race")] + coefficients(mod)[c("time.served")] * 3 + 
    coefficients(mod)[c("max.sentence")] * 12 + coefficients(mod)[c("multiple.offenses")] * 
    0 + coefficients(mod)[c("crime2")]))
p = odds/(1 + odds)
```


## *Evaluating the Model on the Testing Set*

```r
# Predict the probability of violation for the test set
testPred <- predict(mod, newdata = test, type = "response")

# Confusion Matrix
table(test$violator, testPred > 0.5)
```

```
##    
##     FALSE TRUE
##   0   167   12
##   1    11   12
```

```r
# Sensitivity, Specificity and Accuracy
12/23
```

```
## [1] 0.5217
```

```r
167/179
```

```
## [1] 0.933
```

```r
179/202
```

```
## [1] 0.8861
```

```r

# Baseline model
table(test$violator)
```

```
## 
##   0   1 
## 179  23
```

```r
# Accuracy: there are 179 negative examples, which are the ones that the
# baseline model would get correct.
179/202
```

```
## [1] 0.8861
```

```r

# Load ROCR library.  The AUC deals with differentiating between a
# randomly selected positive and negative example. It is independent of
# the regression cutoff selected.
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
ROCRpred <- prediction(testPred, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
```

```
## [1] 0.8946
```

If the board used the model for parole decisions, a negative prediction would lead to a prisoner being granted parole, while a positive prediction would lead to a prisoner being denied parole. The parole board would experience more regret for releasing a prisoner who then violates parole (a negative prediction that is actually positive, or false negative) than it would experience for denying parole to a prisoner who would not have violated parole (a positive prediction that is actually negative, or false positive).

Decreasing the cutoff leads to more positive predictions, which increases false positives and decreases false negatives. Meanwhile, increasing the cutoff leads to more negative predictions, which increases false negatives and decreases false positives. The parole board assigns high cost to false negatives, and therefore should decrease the cutoff. 

## *Identifying Bias in Observational Data*
While expanding the dataset to include the missing parolees and labeling each as violator=0 would improve the representation of non-violators, it does not capture the true outcome, since the parolee might become a violator after 2004. Though labeling these new examples with violator=NA correctly identifies that we don't know their true outcome, we cannot train or test a prediction model with a missing dependent variable.

As a result, a prospective dataset that tracks a cohort of parolees and observes the true outcome of each is more desirable. Unfortunately, such datasets are often more challenging to obtain (for instance, if a parolee had a 10-year term, it might require tracking that individual for 10 years before building the model). Such a prospective analysis would not be possible using the 2004 National Corrections Reporting Program dataset. 
