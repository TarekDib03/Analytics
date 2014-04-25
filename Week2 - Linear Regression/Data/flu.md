Detecting Flu Epidemics via Search Engine Query Data, Analytics Edge, MITx: 15.073x
========================================================
### Tarek Dib

## *Intoduction*
Flu epidemics constitute a major public health concern causing respiratory illnesses, hospitalizations, and deaths. According to the National Vital Statistics Reports published in October 2012, influenza ranked as the eighth leading cause of death in 2011 in the United States. Each year, 250,000 to 500,000 deaths are attributed to influenza related diseases throughout the world.

The U.S. Centers for Disease Control and Prevention (CDC) and the European Influenza Surveillance Scheme (EISS) detect influenza activity through virologic and clinical data, including Influenza-like Illness (ILI) physician visits. Reporting national and regional data, however, are published with a 1-2 week lag.

The Google Flu Trends project was initiated to see if faster reporting can be made possible by considering flu-related online search queries -- data that is available almost immediately.

We would like to estimate influenza-like illness (ILI) activity using Google web search logs. Fortunately, one can easily access this data online:

ILI Data - The CDC publishes on its website the official regional and state-level percentage of patient visits to healthcare providers for ILI purposes on a weekly basis.

Google Search Queries - Google Trends allows public retrieval of weekly counts for every query searched by users around the world. For each location, the counts are normalized by dividing the count for each query in a particular week by the total number of online search queries submitted in that location during the week. Then, the values are adjusted to be between 0 and 1. 

## *Variables*
    "Week" - The range of dates represented by this observation, in year/month/day format.
    "ILI" - This column lists the percentage of ILI-related physician visits for the corresponding week.
    "Queries" - This column lists the fraction of queries that are ILI-related for the corresponding week, adjusted to be between 0 and 1 (higher values correspond to more ILI-related search queries).


## *Understanding the Data*

```r
fluTrain <- read.csv("FluTrain.csv")
fluTest <- read.csv("FluTest.csv")
attach(fluTrain)
# Week corresponds to the highest percentage of ILI-related physician
# visits
levels(factor(Week[which.max(ILI)]))
```

```
## [1] "2009-10-18 - 2009-10-24"
```

```r
# Week that corresponds to the highest percentage of ILI-related query
# fraction
levels(factor(Week[which.max(Queries)]))
```

```
## [1] "2009-10-18 - 2009-10-24"
```



```r
hist(ILI, main = "Distribution of Influenza Like Illness related Physician Visits", 
    xlab = "", col = "blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



```r
plot(log(ILI), Queries)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


## *Linear Regression Model*

```r
FluTrend1 <- lm(log(ILI) ~ Queries, data = fluTrain)
summary(FluTrend1)
```

```
## 
## Call:
## lm(formula = log(ILI) ~ Queries, data = fluTrain)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.7600 -0.1970 -0.0166  0.1868  1.0645 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -0.4993     0.0304   -16.4   <2e-16 ***
## Queries       2.9613     0.0931    31.8   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.3 on 415 degrees of freedom
## Multiple R-squared: 0.709,	Adjusted R-squared: 0.708 
## F-statistic: 1.01e+03 on 1 and 415 DF,  p-value: <2e-16
```

```r
# Correlation. Correlation^2 = R-squared
cor(log(ILI), Queries)
```

```
## [1] 0.842
```

```r
PredTest1 = exp(predict(FluTrend1, newdata = fluTest))
# estimate for the percentage of ILI-related physician visits for the week
# of March 11, 2012
PredTest1[which(fluTest$Week == "2012-03-11 - 2012-03-17")]
```

```
##    11 
## 2.187
```

```r

SSE = sum((PredTest1 - fluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(fluTest))
RMSE
```

```
## [1] 0.7491
```

```r
# Or alternatively
sqrt(mean((PredTest1 - fluTest$ILI)^2))
```

```
## [1] 0.7491
```


## *Training a Time Series Model*
The observations in this dataset are consecutive weekly measurements of the dependent and independent variables. This sort of dataset is called a "time series." Often, statistical models can be improved by predicting the current value of the dependent variable using the value of the dependent variable from earlier weeks. In our models, this means we will predict the ILI variable in the current week using values of the ILI variable from previous weeks.

First, we need to decide the amount of time to lag the observations. Because the ILI variable is reported with a 1- or 2-week lag, a decision maker cannot rely on the previous week's ILI value to predict the current week's value. Instead, the decision maker will only have data available from 2 or more weeks ago. We will build a variable called ILILag2 that contains the ILI value from 2 weeks before the current observation.

To do so, we will use the "zoo" package, which provides a number of helpful methods for time series models. While many functions are built into R, you need to add new packages to use some functions. New packages can be installed and loaded easily in R, and we will do this many times in this class. Run the following two commands to install and load the zoo package. In the first command, you will be prompted to select a CRAN mirror to use for your download. Select a mirror near you geographically.


```r
library(zoo)
```

```
## 
## Attaching package: 'zoo'
## 
## The following object(s) are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad = TRUE)
fluTrain$ILILag2 = coredata(ILILag2)
# In these commands, the value of -2 passed to lag means to return 2
# observations before the current one; a positive value would have
# returned future observations. The parameter na.pad=TRUE means to add
# missing values for the first two weeks of our dataset, where we can't
# compute the data from 2 weeks earlier.

# Missing values in the new variable ILILag2
length(ILILag2) - length(na.omit(ILILag2))
```

```
## [1] 2
```



```r
plot(log(ILI), log(ILILag2), col = "blue")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



```r
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = fluTrain)
summary(FluTrend2)
```

```
## 
## Call:
## lm(formula = log(ILI) ~ Queries + log(ILILag2), data = fluTrain)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.5221 -0.1108 -0.0182  0.0814  0.7678 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -0.2406     0.0195   -12.3   <2e-16 ***
## Queries        1.2558     0.0791    15.9   <2e-16 ***
## log(ILILag2)   0.6557     0.0225    29.1   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.17 on 412 degrees of freedom
##   (2 observations deleted due to missingness)
## Multiple R-squared: 0.906,	Adjusted R-squared: 0.906 
## F-statistic: 1.99e+03 on 2 and 412 DF,  p-value: <2e-16
```


## *Evaluating the Time Series Model in the Test Set*

```r
ILILag2 = lag(zoo(fluTest$ILI), -2, na.pad = TRUE)
fluTest$ILILag2 = coredata(ILILag2)
length(fluTest$ILILag2) - length(na.omit(fluTest$ILILag2))
```

```
## [1] 2
```

```r
# In this problem, the training and testing sets are split sequentially --
# the training set contains all observations from 2004-2011 and the
# testing set contains all observations from 2012. There is no time gap
# between the two datasets, meaning the first observation in FluTest was
# recorded one week after the last observation in FluTrain. From this, we
# can identify how to fill in the missing values for the ILILag2 variable
# in FluTest. So fill in the values of the first 2 observations of ILILag2
# in fluTest set with the last 2 observations in the training set.
fluTest$ILILag2[1] <- fluTrain$ILI[nrow(fluTrain) - 1]
fluTest$ILILag2[2] <- fluTrain$ILI[nrow(fluTrain)]

PredTest2 = exp(predict(FluTrend2, newdata = fluTest))
# RMSE
sqrt(mean((PredTest2 - fluTest$ILI)^2))
```

```
## [1] 0.2942
```
