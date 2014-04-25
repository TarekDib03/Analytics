MITx: 15.071x The Analytics Edge - PREDICTING MEDICAL COSTS WITH CLUSTER-THEN-PREDICT
========================================================
# Name: Tarek Dib
# Date: 04/22/2014

## *Introduction*
In the second lecture sequence this week, we heard about cluster-then-predict, a methodology in which you first cluster observations and then build cluster-specific prediction models. In the lecture sequence, we saw how this methodology helped improve the prediction of heart attack risk. In this assignment, we'll use cluster-then-predict to predict future medical costs using medical claims data.

In Week 4, we discussed the importance of high-quality predictions of future medical costs based on information available in medical claims data. In this problem, you will predict future medical claims using part of the DE-SynPUF dataset, published by the United States Centers for Medicare and Medicaid Services (CMS). This dataset, available in reimbursement.csv, is structured to represent a sample of patients in the Medicare program, which provides health insurance to Americans aged 65 and older as well as some younger people with certain medical conditions. To protect the privacy of patients represented in this publicly available dataset, CMS performs a number of steps to anonymize the data, so we would need to re-train the models we develop in this problem on de-anonymized data if we wanted to apply our models in the real world.

The observations in the dataset represent a 1% random sample of Medicare beneficiaries in 2008, limited to those still alive at the end of 2008. The dependent variable, reimbursement2009, represents the total value of all Medicare reimbursements for a patient in 2009, which is the cost of the patient's care to the Medicare system. The following independent variables are available:

    age: The patient's age in years at the beginning of 2009
    alzheimers: Binary variable for whether the patient had diagnosis codes for Alzheimer's       disease or a related disorder in 2008
    arthritis: Binary variable for whether the patient had diagnosis codes for rheumatoid arthritis or osteoarthritis in 2008
    cancer: Binary variable for whether the patient had diagnosis codes for cancer in 2008
    copd: Binary variable for whether the patient had diagnosis codes for Chronic Obstructive Pulmonary Disease (COPD) in 2008
    depression: Binary variable for whether the patient had diagnosis codes for depression in 2008
    diabetes: Binary variable for whether the patient had diagnosis codes for diabetes in 2008
    heart.failure: Binary variable for whether the patient had diagnosis codes for heart failure in 2008
    ihd: Binary variable for whether the patient had diagnosis codes for ischemic heart disease (IHD) in 2008
    kidney: Binary variable for whether the patient had diagnosis codes for chronic kidney disease in 2008
    osteoporosis: Binary variable for whether the patient had diagnosis codes for osteoporosis in 2008
    stroke: Binary variable for whether the patient had diagnosis codes for a stroke/transient ischemic attack (TIA) in 2008
    reimbursement2008: The total amount of Medicare reimbursements for this patient for 2008

## *PREPARING THE DATASET*

```r
# Data
setwd("/home/tarek/Analytics/Weeks/Week6-Clustering/Data")
claims <- read.csv("reimbursement.csv")

# Subset of claims. Patients who had at least one of the chronic conditions
has.condition = subset(claims, alzheimers == 1 | arthritis == 1 | cancer == 
    1 | copd == 1 | depression == 1 | diabetes == 1 | heart.failure == 1 | ihd == 
    1 | kidney == 1 | osteoporosis == 1 | stroke == 1)
# Ratio
nrow(has.condition)/nrow(claims)
```

```
## [1] 0.6123
```

```r
# Maximum correlation between independent variables
sort(cor(claims[-(13:14)]))[132]
```

```
## [1] 0.5146
```

```r

# Distribution of reimbursement in 2008
hist(claims$reimbursement2008)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r

# Transform the dependent variables. +1 so that we don't get log(0) which is
# -infinity
claims$reimbursement2008 = log(claims$reimbursement2008 + 1)
claims$reimbursement2009 = log(claims$reimbursement2009 + 1)

hist(claims$reimbursement2009, main = "Log of Reimbursement in 2009")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 

```r
# About 20% of beneficiaries had $0 reimbursement in 2009
sum(claims$reimbursement2009 == 0)/length(claims$reimbursement2009)
```

```
## [1] 0.1976
```


## *INITIAL LINEAR REGRESSION MODEL*

```r
set.seed(144)
spl = sample(1:nrow(claims), size = 0.7 * nrow(claims))
train = claims[spl, ]
test = claims[-spl, ]

lm.claims <- lm(reimbursement2009 ~ ., data = train)
summary(lm.claims)
```

```
## 
## Call:
## lm(formula = reimbursement2009 ~ ., data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.708  -1.428  -0.062   0.887   9.382 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        1.857321   0.019604   94.74  < 2e-16 ***
## age               -0.001014   0.000262   -3.87  0.00011 ***
## alzheimers        -0.015690   0.009434   -1.66  0.09628 .  
## arthritis          0.047807   0.009933    4.81  1.5e-06 ***
## cancer            -0.040556   0.013893   -2.92  0.00351 ** 
## copd              -0.185806   0.010966  -16.94  < 2e-16 ***
## depression         0.089939   0.009008    9.98  < 2e-16 ***
## diabetes           0.251835   0.008958   28.11  < 2e-16 ***
## heart.failure      0.008988   0.009132    0.98  0.32501    
## ihd                0.154717   0.008999   17.19  < 2e-16 ***
## kidney            -0.226933   0.010635  -21.34  < 2e-16 ***
## osteoporosis       0.105285   0.009271   11.36  < 2e-16 ***
## stroke            -0.254776   0.016667  -15.29  < 2e-16 ***
## reimbursement2008  0.759142   0.001407  539.69  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.85 on 320589 degrees of freedom
## Multiple R-squared:  0.692,	Adjusted R-squared:  0.692 
## F-statistic: 5.55e+04 on 13 and 320589 DF,  p-value: <2e-16
```

```r

# MSE
predTest <- predict(lm.claims, newdata = test)
SSE <- sum((predTest - test$reimbursement2009)^2)
rms.lm <- sqrt(SSE/nrow(test))
rms.lm
```

```
## [1] 1.849
```

```r

# The naive baseline predicts the average of the dependent variable
# (reimbursement2009) on the training set! baseline model is
# mean(train$reimbursement2009). The testing MSE is:
rms.baseline <- sqrt(mean((mean(train$reimbursement2009) - test$reimbursement2009)^2))
rms.baseline
```

```
## [1] 3.335
```

```r

# Smart baseline model: predict that a patient's medical costs would be
# equal to their costs in the previous year
smartModel <- train$reimbursement2008
# RMSE
sqrt(mean((test$reimbursement2008 - test$reimbursement2009)^2))
```

```
## [1] 2.095
```


## *CLUSTERING MEDICARE BENEFICIARIES*

```r
# remove the dependent variable using the following commands:
train.limited = train
train.limited$reimbursement2009 = NULL
test.limited = test
test.limited$reimbursement2009 = NULL

# In cluster-then-predict, our final goal is to predict the dependent
# variable, which is unknown to us at the time of prediction. Therefore, if
# we need to know the outcome value to perform the clustering, the
# methodology is no longer useful for prediction of an unknown outcome
# value.

# This is an important point that is sometimes mistakenly overlooked. If you
# use the outcome value to cluster, you might conclude your method strongly
# outperforms a non-clustering alternative. However, this is because it is
# using the outcome to determine the clusters, which is not valid.

# Normalize
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)
mean(train.norm$arthritis)
```

```
## [1] 2.049e-17
```


## *K-means Clustering*

```r
k = 3
set.seed(144)
km <- kmeans(train.norm, centers = k)
kmClust <- km$cluster
table(kmClust)
```

```
## kmClust
##      1      2      3 
##  55780 146396 118427
```

```r

km$centers
```

```
##        age alzheimers arthritis   cancer    copd depression diabetes
## 1  0.14799    1.01893    0.8219  0.63518  1.3529     0.8948   1.0260
## 2 -0.10237   -0.43237   -0.3959 -0.23297 -0.3746    -0.4447  -0.7613
## 3  0.05684    0.05456    0.1023 -0.01118 -0.1742     0.1282   0.4578
##   heart.failure     ihd  kidney osteoporosis  stroke reimbursement2008
## 1        1.1909  0.9646  1.4830       0.5605  0.8531            0.9973
## 2       -0.5741 -0.8342 -0.4219      -0.3907 -0.2059           -0.8435
## 3        0.1487  0.5769 -0.1769       0.2189 -0.1473            0.5730
```

```r

# Use the flexclust package to obtain training set and testing set cluster
# assignments for our observations
library(flexclust)
```

```
## Loading required package: grid
## Loading required package: modeltools
## Loading required package: stats4
```

```r
km.kcca = as.kcca(km, train.norm)
cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata = test.norm)
sum(cluster.test == 2)
```

```
## [1] 62651
```


## *CLUSTER-SPECIFIC PREDICTIONS*

```r
train1 <- subset(train, cluster.train == 1)
train2 <- subset(train, cluster.train == 2)
train3 <- subset(train, cluster.train == 3)
mean(train1$reimbursement2009)
```

```
## [1] 8.724
```

```r
mean(train2$reimbursement2009)
```

```
## [1] 3.661
```

```r
mean(train3$reimbursement2009)
```

```
## [1] 7.882
```

```r

test1 = subset(test, cluster.test == 1)
test2 = subset(test, cluster.test == 2)
test3 = subset(test, cluster.test == 3)

lm1 <- lm(reimbursement2009 ~ ., data = train1)
lm2 <- lm(reimbursement2009 ~ ., data = train2)
lm3 <- lm(reimbursement2009 ~ ., data = train3)
summary(lm1)
```

```
## 
## Call:
## lm(formula = reimbursement2009 ~ ., data = train1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.374 -0.645 -0.102  0.669  3.547 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        6.128373   0.045509  134.66  < 2e-16 ***
## age               -0.000778   0.000332   -2.34    0.019 *  
## alzheimers         0.048863   0.009049    5.40  6.7e-08 ***
## arthritis          0.127756   0.008960   14.26  < 2e-16 ***
## cancer             0.167001   0.010825   15.43  < 2e-16 ***
## copd               0.101623   0.009457   10.75  < 2e-16 ***
## depression         0.140563   0.008948   15.71  < 2e-16 ***
## diabetes           0.279173   0.013516   20.66  < 2e-16 ***
## heart.failure      0.163891   0.011716   13.99  < 2e-16 ***
## ihd                0.183921   0.014718   12.50  < 2e-16 ***
## kidney             0.179917   0.010407   17.29  < 2e-16 ***
## osteoporosis       0.064333   0.009030    7.12  1.1e-12 ***
## stroke             0.046074   0.010844    4.25  2.2e-05 ***
## reimbursement2008  0.185733   0.004539   40.92  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.03 on 55766 degrees of freedom
## Multiple R-squared:  0.0962,	Adjusted R-squared:  0.096 
## F-statistic:  457 on 13 and 55766 DF,  p-value: <2e-16
```

```r
summary(lm2)
```

```
## 
## Call:
## lm(formula = reimbursement2009 ~ ., data = train2)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.247  -1.653  -0.617   1.393   9.471 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        1.795041   0.036950   48.58  < 2e-16 ***
## age               -0.002001   0.000506   -3.95  7.8e-05 ***
## alzheimers         0.298556   0.043913    6.80  1.1e-11 ***
## arthritis          0.553770   0.059267    9.34  < 2e-16 ***
## cancer             0.423085   0.076207    5.55  2.8e-08 ***
## copd               0.217739   0.072533    3.00   0.0027 ** 
## depression         0.416892   0.037593   11.09  < 2e-16 ***
## diabetes           0.824799   0.062367   13.22  < 2e-16 ***
## heart.failure      0.314764   0.040683    7.74  1.0e-14 ***
## ihd                0.263823   0.067988    3.88   0.0001 ***
## kidney             0.132773   0.081331    1.63   0.1026    
## osteoporosis       0.467177   0.040381   11.57  < 2e-16 ***
## stroke             0.003150   0.133760    0.02   0.9812    
## reimbursement2008  0.770075   0.002401  320.71  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.39 on 146383 degrees of freedom
## Multiple R-squared:  0.507,	Adjusted R-squared:  0.507 
## F-statistic: 1.16e+04 on 13 and 146383 DF,  p-value: <2e-16
```

```r
summary(lm3)
```

```
## 
## Call:
## lm(formula = reimbursement2009 ~ ., data = train3)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.098 -0.577 -0.062  0.570  4.302 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       5.072540   0.033288  152.38   <2e-16 ***
## age               0.000487   0.000275    1.77    0.076 .  
## alzheimers        0.073427   0.008363    8.78   <2e-16 ***
## arthritis         0.234484   0.008828   26.56   <2e-16 ***
## cancer            0.277148   0.014359   19.30   <2e-16 ***
## copd              0.144986   0.012966   11.18   <2e-16 ***
## depression        0.136755   0.007778   17.58   <2e-16 ***
## diabetes          0.229013   0.007058   32.45   <2e-16 ***
## heart.failure     0.113918   0.007162   15.91   <2e-16 ***
## ihd               0.142788   0.007608   18.77   <2e-16 ***
## kidney            0.189291   0.011890   15.92   <2e-16 ***
## osteoporosis      0.094386   0.007774   12.14   <2e-16 ***
## stroke            0.033999   0.028590    1.19    0.234    
## reimbursement2008 0.308355   0.003738   82.49   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.16 on 118412 degrees of freedom
## Multiple R-squared:  0.118,	Adjusted R-squared:  0.117 
## F-statistic: 1.21e+03 on 13 and 118412 DF,  p-value: <2e-16
```

```r

pred.Test1 <- predict(lm1, newdata = test1)
pred.Test2 <- predict(lm2, newdata = test2)
pred.Test3 <- predict(lm3, newdata = test3)

pred.TestMeans <- c(mean(pred.Test1), mean(pred.Test2), mean(pred.Test3))
names(pred.TestMeans) = c("pred.Test1", "pred.Test2", "pred.Test3")
# Which vector of test-set predictions has the smallest average predicted
# reimbursement amount?
which.min(pred.TestMeans)
```

```
## pred.Test2 
##          2
```

```r

# Obtain the test-set RMSE for each cluster
RMSE.Test1 <- sqrt(mean((pred.Test1 - test1$reimbursement2009)^2))
RMSE.Test2 <- sqrt(mean((pred.Test2 - test2$reimbursement2009)^2))
RMSE.Test3 <- sqrt(mean((pred.Test3 - test3$reimbursement2009)^2))
RMSE.Test <- c(RMSE.Test1, RMSE.Test2, RMSE.Test3)
names(RMSE.Test) = c(RMSE.Test1, RMSE.Test2, RMSE.Test3)
# Which cluster has the largest test-set RMSE?
which.max(RMSE.Test)
```

```
## 2.38315594869223 
##                2
```

```r

# To compute the overall test-set RMSE of the cluster-then-predict approach,
# we can combine all the test-set predictions into a single vector and all
# the true outcomes into a single vector.
all.predictions = c(pred.Test1, pred.Test2, pred.Test3)
all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)
# What is the test-set RMSE of the cluster-then-predict approach
sqrt(mean((all.predictions - all.outcomes)^2))
```

```
## [1] 1.811
```


We see a modest improvement over the original linear regression model, which is typical in situations where the observations do not cluster strongly into different "types" of observations. However, it is often a good idea to try the cluster-then-predict approach on datasets with a large number of observations to see if you can improve the accuracy of your model.
