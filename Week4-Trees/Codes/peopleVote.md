MITx: 15.071x The Analytics Edge - Why People Vote
========================================================
### Tarek Dib
### April 7, 2014

# *Introduction*
In August 2006 three researchers (Alan Gerber and Donald Green of Yale University, and Christopher Larimer of the University of Northern Iowa) carried out a large scale field experiment in Michigan, USA to test the hypothesis that one of the reasons people vote is social, or extrinsic, pressure. To quote the first paragraph of their 2008 research paper:

    Among the most striking features of a democratic political system is the participation of millions of voters in elections. Why do large numbers of people vote, despite the fact that ... "the casting of a single vote is of no significance where there is a multitude of electors"? One hypothesis is adherence to social norms. Voting is widely regarded as a citizen duty, and citizens worry that others will think less of them if they fail to participate in elections. Voters' sense of civic duty has long been a leading explanation of vote turnout...

In this problem we will use both logistic regression and classification trees to analyze the data they collected.

# *Understanding the Data*
The researchers grouped the 344,000 voters into different groups randomly - 191,000 voters were a "control" group, and the rest were categorized into one of four "treatment" groups. These five groups correspond to five binary variables in the dataset.

    "Civic Duty" (variable civicduty) group members were sent a letter that simply said "DO YOUR CIVIC DUTY - VOTE!"
    "Hawthorne Effect" (variable hawthorne) group members were sent a letter that had the "Civic Duty" message plus the additional message "YOU ARE BEING STUDIED" and they were informed that their voting behavior would be examined by means of public records.
    "Self" (variable self) group members received the "Civic Duty" message as well as the recent voting record of everyone in that household and a message stating that another message would be sent after the election with updated records.
    "Neighbors" (variable neighbors) group members were given the same message as that for the "Self" group, except the message not only had the household voting records but also that of neighbors - maximizing social pressure.
    "Control" (variable control) group members were not sent anything, and represented the typical voting situation.
    Additional variables include sex (0 for male, 1 for female), yob (year of birth), and the dependent variable voting (1 if they voted, 0 otherwise).
# *Exploratoration and Logistic Regression Model*

```r
# Read Data
gerber = read.csv("gerber.csv")
# Proportion of voters in each category (0 or 1)
table(gerber$voting)/sum(table(gerber$voting))
```

```
## 
##      0      1 
## 0.6841 0.3159
```

```r
# Treatment group that had the largest fraction of voters. compute the
# mean value of 'voting', sorted by whether or not the people were in each
# group
tapply(gerber$voting, gerber$civicduty, mean)
```

```
##      0      1 
## 0.3161 0.3145
```

```r
tapply(gerber$voting, gerber$hawthorne, mean)
```

```
##      0      1 
## 0.3151 0.3224
```

```r
tapply(gerber$voting, gerber$self, mean)
```

```
##      0      1 
## 0.3122 0.3452
```

```r
tapply(gerber$voting, gerber$neighbors, mean)
```

```
##      0      1 
## 0.3082 0.3779
```

```r
# Building a logistic model using all the data
mod1 <- glm(voting ~ hawthorne + self + neighbors + civicduty, data = gerber, 
    family = "binomial")
summary(mod1)
```

```
## 
## Call:
## glm(formula = voting ~ hawthorne + self + neighbors + civicduty, 
##     family = "binomial", data = gerber)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -0.974  -0.869  -0.839   1.459   1.559  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -0.86336    0.00501 -172.46  < 2e-16 ***
## hawthorne    0.12048    0.01204   10.01  < 2e-16 ***
## self         0.22294    0.01187   18.79  < 2e-16 ***
## neighbors    0.36509    0.01168   31.26  < 2e-16 ***
## civicduty    0.08437    0.01210    6.97  3.1e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 429238  on 344083  degrees of freedom
## Residual deviance: 428090  on 344079  degrees of freedom
## AIC: 428100
## 
## Number of Fisher Scoring iterations: 4
```

```r
# Predict
pred1 = predict(mod1, type = "response")
t1_0.3 <- table(gerber$voting, pred1 >= 0.3)
t1_0.5 <- table(gerber$voting, pred1 >= 0.5)
# Accuracy
(t1_0.3[1, 1] + t1_0.3[2, 2])/sum(t1_0.3)
```

```
## [1] 0.542
```

```r
t1_0.5[1]/sum(t1_0.5)
```

```
## [1] 0.6841
```

```r
# Baseline model
t_bl <- table(gerber$voting)
# AUC
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
ROCRpred = prediction(pred1, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)
```

```
## [1] 0.5308
```

Even though all of the variables in the logistic regression model are significant, our model does not improve over the baseline model of just predicting that someone will not vote, and the AUC is low. So while the treatment groups do make a difference, this is a weak predictive model.

# *Tree Models*
We will now try out trees. We will build a CART tree for voting using all data and the same four treatment variables we used before. We will not set the option method="class" - we are actually going to create a regression tree here, and not a classification one. We are interested in building a tree to explore the fraction of people who vote, or the probability of voting. We’d like CART to split our groups if they have different probabilities of voting. If we used method=‘class’, CART would only split if one of the groups had a probability of voting above 50% and the other had a probability of voting less than 50% (since the predicted outcomes would be different). However, with regression trees, CART will split even if both groups have probability less than 50%.

```r
# Regression Tree model
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)

# Another regression tree model
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, 
    cp = 0)
prp(CARTmodel2)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r

# Include sex variable in the CART model
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, 
    data = gerber, cp = 0)
prp(CARTmodel3)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 

From model3, it is shown from the tree diagram that men are more likely to vote than women!

# *Interaction Terms*

```r
CARTmodel4 = rpart(voting ~ control + sex, data = gerber, cp = 0)
prp(CARTmodel4, digits = 6)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
