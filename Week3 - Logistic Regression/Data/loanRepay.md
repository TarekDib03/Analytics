MITx: 15.071x The Analytics Edge
------------------------------------------------------------------
### Predicting loan repayment and Risk
#### Tarek Dib
#### Date: March 24, 2014

## *Introduction*

In the lending industry, investors provide loans to borrowers in exchange for the promise of repayment with interest. If the borrower repays the loan, then the lender profits from the interest. However, if the borrower is unable to repay the loan, then the lender loses money. Therefore, lenders face the problem of predicting the risk of a borrower being unable to repay a loan.

To address this problem, we will use publicly available data from LendingClub.com, a website that connects borrowers and investors over the Internet. This dataset represents 9,578 3-year loans that were funded through the LendingClub.com platform between May 2007 and February 2010. The binary dependent variable "not_fully_paid" indicates that the loan was not paid back in full (the borrower either defaulted or the loan was "charged off," meaning the borrower was deemed unlikely to ever pay it back).

To predict this dependent variable, we will use the following independent variables available to the investor when deciding whether to fund a loan:

    credit.policy: 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise.
    purpose: The purpose of the loan (takes values "credit_card", "debt_consolidation", "educational", "major_purchase", "small_business", and "all_other").
    int.rate: The interest rate of the loan, as a proportion (a rate of 11% would be stored as 0.11). Borrowers judged by LendingClub.com to be more risky are assigned higher interest rates.
    installment: The monthly installments ($) owed by the borrower if the loan is funded.
    log.annual.inc: The natural log of the self-reported annual income of the borrower.
    dti: The debt-to-income ratio of the borrower (amount of debt divided by annual income).
    fico: The FICO credit score of the borrower.
    days.with.cr.line: The number of days the borrower has had a credit line.
    revol.bal: The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle).
    revol.util: The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available).
    inq.last.6mths: The borrower's number of inquiries by creditors in the last 6 months.
    delinq.2yrs: The number of times the borrower had been 30+ days past due on a payment in the past 2 years.
    pub.rec: The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments).

## *Preparing the Dataset*

```r
loans <- read.csv("loans.csv")
str(loans)
```

```
## 'data.frame':	9578 obs. of  14 variables:
##  $ credit.policy    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ purpose          : Factor w/ 7 levels "all_other","credit_card",..: 3 2 3 3 2 2 3 1 5 3 ...
##  $ int.rate         : num  0.119 0.107 0.136 0.101 0.143 ...
##  $ installment      : num  829 228 367 162 103 ...
##  $ log.annual.inc   : num  11.4 11.1 10.4 11.4 11.3 ...
##  $ dti              : num  19.5 14.3 11.6 8.1 15 ...
##  $ fico             : int  737 707 682 712 667 727 667 722 682 707 ...
##  $ days.with.cr.line: num  5640 2760 4710 2700 4066 ...
##  $ revol.bal        : int  28854 33623 3511 33667 4740 50807 3839 24220 69909 5630 ...
##  $ revol.util       : num  52.1 76.7 25.6 73.2 39.5 51 76.8 68.6 51.1 23 ...
##  $ inq.last.6mths   : int  0 0 1 1 0 0 0 0 1 1 ...
##  $ delinq.2yrs      : int  0 0 0 0 1 0 0 0 0 0 ...
##  $ pub.rec          : int  0 0 0 0 0 0 1 0 0 0 ...
##  $ not.fully.paid   : int  0 0 0 0 0 0 1 1 0 0 ...
```

```r
summary(loans)
```

```
##  credit.policy                 purpose        int.rate      installment   
##  Min.   :0.000   all_other         :2331   Min.   :0.060   Min.   : 15.7  
##  1st Qu.:1.000   credit_card       :1262   1st Qu.:0.104   1st Qu.:163.8  
##  Median :1.000   debt_consolidation:3957   Median :0.122   Median :268.9  
##  Mean   :0.805   educational       : 343   Mean   :0.123   Mean   :319.1  
##  3rd Qu.:1.000   home_improvement  : 629   3rd Qu.:0.141   3rd Qu.:432.8  
##  Max.   :1.000   major_purchase    : 437   Max.   :0.216   Max.   :940.1  
##                  small_business    : 619                                  
##  log.annual.inc       dti             fico     days.with.cr.line
##  Min.   : 7.55   Min.   : 0.00   Min.   :612   Min.   :  179    
##  1st Qu.:10.56   1st Qu.: 7.21   1st Qu.:682   1st Qu.: 2820    
##  Median :10.93   Median :12.66   Median :707   Median : 4140    
##  Mean   :10.93   Mean   :12.61   Mean   :711   Mean   : 4562    
##  3rd Qu.:11.29   3rd Qu.:17.95   3rd Qu.:737   3rd Qu.: 5730    
##  Max.   :14.53   Max.   :29.96   Max.   :827   Max.   :17640    
##  NA's   :4                                     NA's   :29       
##    revol.bal         revol.util    inq.last.6mths   delinq.2yrs    
##  Min.   :      0   Min.   :  0.0   Min.   : 0.00   Min.   : 0.000  
##  1st Qu.:   3187   1st Qu.: 22.7   1st Qu.: 0.00   1st Qu.: 0.000  
##  Median :   8596   Median : 46.4   Median : 1.00   Median : 0.000  
##  Mean   :  16914   Mean   : 46.9   Mean   : 1.57   Mean   : 0.164  
##  3rd Qu.:  18250   3rd Qu.: 71.0   3rd Qu.: 2.00   3rd Qu.: 0.000  
##  Max.   :1207359   Max.   :119.0   Max.   :33.00   Max.   :13.000  
##                    NA's   :62      NA's   :29      NA's   :29      
##     pub.rec      not.fully.paid
##  Min.   :0.000   Min.   :0.00  
##  1st Qu.:0.000   1st Qu.:0.00  
##  Median :0.000   Median :0.00  
##  Mean   :0.062   Mean   :0.16  
##  3rd Qu.:0.000   3rd Qu.:0.00  
##  Max.   :5.000   Max.   :1.00  
##  NA's   :29
```

```r

# Variables with at least one missing observation
names(which(sapply(loans, function(x) sum(is.na(x)) >= 1) == "TRUE"))
```

```
## [1] "log.annual.inc"    "days.with.cr.line" "revol.util"       
## [4] "inq.last.6mths"    "delinq.2yrs"       "pub.rec"
```

```r
# Proportion of not fully paid loans
as.numeric(table(loans$not.fully.paid)/nrow(loans))[2]
```

```
## [1] 0.1601
```

```r

# Data frame with missing observations
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | 
    is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)  # removing this small number of observations would not lead to overfitting
```

```
## [1] 62
```

```r
# Proportion of not fully paid in the missing data frame
as.numeric(table(missing$not.fully.paid)/nrow(missing))[2]  # This rate is similar to the 16.01% across all loans, so the form of biasing described is not an issue
```

```
## [1] 0.1935
```

```r

# Load VIM library
library(VIM)
```

```
## Loading required package: colorspace
## Loading required package: grid
## VIM is ready to use. 
##  Since version 4.0.0 the GUI is in its own package VIMGUI.
## 
##           Please use the package to use the new (and old) GUI.
## 
## 
## Attaching package: 'VIM'
## 
## The following object(s) are masked from 'package:datasets':
## 
##     sleep
```

```r
set.seed(144)
# Set variables to be imputed. All the predictors vars.for.imputation =
# setdiff(names(loans), 'not.fully.paid') imputed =
# irmi(loans[vars.for.imputation]) loans[vars.for.imputation] = imputed
# summary(loans) Read imputed data set to compare to the above imputed
# data
loans_imputed <- read.csv("loans_imputed.csv")  # Use this instead of the one developed using VIM
summary(loans_imputed)
```

```
##  credit.policy                 purpose        int.rate      installment   
##  Min.   :0.000   all_other         :2331   Min.   :0.060   Min.   : 15.7  
##  1st Qu.:1.000   credit_card       :1262   1st Qu.:0.104   1st Qu.:163.8  
##  Median :1.000   debt_consolidation:3957   Median :0.122   Median :268.9  
##  Mean   :0.805   educational       : 343   Mean   :0.123   Mean   :319.1  
##  3rd Qu.:1.000   home_improvement  : 629   3rd Qu.:0.141   3rd Qu.:432.8  
##  Max.   :1.000   major_purchase    : 437   Max.   :0.216   Max.   :940.1  
##                  small_business    : 619                                  
##  log.annual.inc       dti             fico     days.with.cr.line
##  Min.   : 7.55   Min.   : 0.00   Min.   :612   Min.   :  179    
##  1st Qu.:10.56   1st Qu.: 7.21   1st Qu.:682   1st Qu.: 2820    
##  Median :10.93   Median :12.66   Median :707   Median : 4140    
##  Mean   :10.93   Mean   :12.61   Mean   :711   Mean   : 4561    
##  3rd Qu.:11.29   3rd Qu.:17.95   3rd Qu.:737   3rd Qu.: 5730    
##  Max.   :14.53   Max.   :29.96   Max.   :827   Max.   :17640    
##                                                                 
##    revol.bal         revol.util    inq.last.6mths   delinq.2yrs    
##  Min.   :      0   Min.   :  0.0   Min.   : 0.00   Min.   : 0.000  
##  1st Qu.:   3187   1st Qu.: 22.6   1st Qu.: 0.00   1st Qu.: 0.000  
##  Median :   8596   Median : 46.3   Median : 1.00   Median : 0.000  
##  Mean   :  16914   Mean   : 46.8   Mean   : 1.58   Mean   : 0.164  
##  3rd Qu.:  18250   3rd Qu.: 70.9   3rd Qu.: 2.00   3rd Qu.: 0.000  
##  Max.   :1207359   Max.   :119.0   Max.   :33.00   Max.   :13.000  
##                                                                    
##     pub.rec      not.fully.paid
##  Min.   :0.000   Min.   :0.00  
##  1st Qu.:0.000   1st Qu.:0.00  
##  Median :0.000   Median :0.00  
##  Mean   :0.062   Mean   :0.16  
##  3rd Qu.:0.000   3rd Qu.:0.00  
##  Max.   :5.000   Max.   :1.00  
## 
```


## *Prediction Models*

```r
loans_imputed$purpose = as.factor(loans_imputed$purpose)
set.seed(144)
# Load caTools to use sample.split function
library(caTools)
split <- sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans_imputed, split == TRUE)
test <- subset(loans_imputed, split == FALSE)

# Build the model using training set
mod <- glm(not.fully.paid ~ ., data = train, family = "binomial")
# index of Significant predictors
id <- which(summary(mod)$coeff[, 4] < 0.05)
# Significant predictors
coeff.sig <- summary(mod)$coeff[, 1][id]
names(coeff.sig)
```

```
##  [1] "(Intercept)"               "credit.policy"            
##  [3] "purposecredit_card"        "purposedebt_consolidation"
##  [5] "purposemajor_purchase"     "purposesmall_business"    
##  [7] "installment"               "log.annual.inc"           
##  [9] "fico"                      "revol.bal"                
## [11] "inq.last.6mths"            "pub.rec"
```

```r

# Predict the probability of not fully paid back using the testing set
predicted.risk <- predict(mod, newdata = test, type = "response")
# Add the predicted.risk variable to the test set
test$predicted.risk <- predicted.risk
# Create the confusion matrix of the test set
Conf <- table(test$not.fully.paid, predicted.risk > 0.5)
# Accuracy of the logistic regression model
(Conf[1, 1] + Conf[2, 2])/sum(Conf)
```

```
## [1] 0.8364
```

```r
# Accuracy of the baseline model

# Load the ROCR to estimate the AUC (Area Under Curve or c statistics)
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
ROCRpred <- prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)
```

```
## [1] 0.6721
```


## *Smart Baseline*
In the previous problem, we built a logistic regression model that has an AUC significantly higher than the AUC of 0.5 that would be obtained by randomly ordering observations.

However, LendingClub.com assigns the interest rate to a loan based on their estimate of that loan's risk. This variable, int.rate, is an independent variable in our dataset. In this part, we will investigate using the loan's interest rate as a "smart baseline" to order the loans according to risk.

```r
# Train a bivariate logistic regression model.
bivariate = glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(bivariate)  # In this model, int.rate is highly significant
```

```
## 
## Call:
## glm(formula = not.fully.paid ~ int.rate, family = "binomial", 
##     data = train)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.055  -0.627  -0.544  -0.436   2.291  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -3.673      0.169   -21.8   <2e-16 ***
## int.rate      15.921      1.270    12.5   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5734.8  on 6703  degrees of freedom
## AIC: 5739
## 
## Number of Fisher Scoring iterations: 4
```

```r
cor(train$int.rate, train$fico)  # Highly correlated. That's why in the multivariate model, int.rate was no significant
```

```
## [1] -0.7117
```

```r

# Make test set predictions for the bivariate model.
bivariate.Pred <- predict(bivariate, newdata = test, type = "response")
# Highest predicted probability of a loan not being paid in full on the
# testing set
max(summary(bivariate.Pred))
```

```
## [1] 0.427
```

```r

# AUC for the Bivariate model
prediction.bivariate <- prediction(bivariate.Pred, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)
```

```
## [1] 0.6239
```


## *A Simple Investment Strategy*
While thus far we have predicted if a loan will be paid back or not, an investor needs to identify loans that are expected to be profitable. If the loan is paid back in full, then the investor makes interest on the loan. However, if the loan is not paid back, the investor loses the money invested. Therefore, the investor should seek loans that best balance this risk and reward.

To compute interest revenue, consider a $c investment in a loan that has an annual interest rate r over a period of t years. Using continuous compounding of interest, this investment pays back c * exp(rt) dollars by the end of the t years.

An investor who invested c dollars in a loan with interest rate r for t years makes c * (exp(rt) - 1) dollars of profit if the loan is paid back in full and -c dollars of profit if the loan is not paid back in full.

In order to evaluate the quality of an investment strategy, we need to compute this profit for each loan in the test set. For this variable, we will assume a $1 investment (aka c=1). To create the variable, we first assign to the profit for a fully paid loan, exp(rt)-1, to every observation, and we then replace this value with -1 in the cases where the loan was not paid in full. All the loans in our dataset are 3-year loans, so t=3.

```r
test$profit = exp(test$int.rate * 3) - 1
# Replace the values of the variable above with -1 in the cases where the
# loan was not paid in full.
test$profit[test$not.fully.paid == 1] = -1
# Maximum profit if a $10 was invested
max(test$profit) * 10
```

```
## [1] 8.895
```

## *An Investment Strategy Based on Risk*
A simple investment strategy of equally investing in all the loans would yield profit $20.94 for a $100 investment. But this simple investment strategy does not leverage the prediction model we built earlier in this problem. As stated earlier, investors seek loans that balance reward with risk, in that they simultaneously have high interest rates and a low risk of not being paid back.

To meet this objective, we will analyze an investment strategy in which the investor only purchases loans with a high interest rate (a rate of at least 15%), but amongst these loans selects the ones with the lowest predicted risk of not being paid back in full. We will model an investor who invests $1 in each of the most promising 100 loans.

```r
# Build a data frame called highInterest consisting of the test set loans
# with an interest rate of at least 15%.
highInterest <- subset(test, test$int.rate >= 0.15)
# Ratio of loans that were not fully paid in the highInterest rate data
# set
as.numeric(table(highInterest$not.fully.paid)/sum(table(highInterest$not.fully.paid)))[2]
```

```
## [1] 0.2517
```

```r

# We will determine the 100th smallest predicted probability of not paying
# in full by sorting the predicted risks in increasing order and selecting
# the 100th element of this sorted list.
cutoff = sort(highInterest$predicted.risk, decreasing = FALSE)[100]
# Build a data frame called selectedLoans consisting of the high-interest
# loans with predicted risk not exceeding the cutoff
selectedLoans <- subset(highInterest, highInterest$predicted.risk <= cutoff)
# Number of loans that were not fully paid in selected 100 loans
table(selectedLoans$not.fully.paid)
```

```
## 
##  0  1 
## 81 19
```

```r
# Or
sum(selectedLoans$profit == -1)
```

```
## [1] 19
```

