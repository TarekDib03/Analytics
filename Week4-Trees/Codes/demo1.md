MITx: 15.071x The Analytics Edge - Regression Trees for Housing Data in Boston
========================================================
### Tarek Dib
### April 6, 2014

# *Introduction*
A paper was written on the relationship between house prices and clean air in the late 1970s by David Harrison of Harvard and Daniel Rubinfeld of U. of Michigan. “Hedonic Housing Prices and the Demand for Clean Air” has been citedmore than 1000 times. Data set was widely used to evaluate algorithms. In this report, we will explore the dataset with the aid of trees, compare linear regression with regression trees, discuss what the “cp” parameter means and apply cross-validation to regression trees.

# *Understanding the Data*
Each entry corresponds to a census tract, a statistical division of the area that is used by researchers to break down towns and cities. There will be multiple census tracts per Town.

### *Variables*
    LON and LAT are the longitude and latitude of the center of the census tract.
    MEDV is the median value of owner-occupied homes, in thousands of dollars.
    CRIM is the per capita crime rate
    ZN is related to how much of the land is zoned for large residential properties
    INDUS is proportion of area used for industry
    CHAS is 1 if the census tract is next to the Charles River
    NOX is the concentration of nitrous oxides in the air
    RM is the average number of rooms per dwelling
    AGE is the proportion of owner-occupied units built before 1940
    DIS is a measure of how far the tract is from centers of employment in Boston
    RAD is a measure of closeness to important highways
    TAX is the property tax rate per $10,000 of value
    PTRATIO is the pupil-teacher ratio by town
# *Exploratory Data Analysis*

```r
# Read Data
boston = read.csv("boston.csv")
str(boston)
```

```
## 'data.frame':	506 obs. of  16 variables:
##  $ TOWN   : Factor w/ 92 levels "Arlington","Ashland",..: 54 77 77 46 46 46 69 69 69 69 ...
##  $ TRACT  : int  2011 2021 2022 2031 2032 2033 2041 2042 2043 2044 ...
##  $ LON    : num  -71 -71 -70.9 -70.9 -70.9 ...
##  $ LAT    : num  42.3 42.3 42.3 42.3 42.3 ...
##  $ MEDV   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 22.1 16.5 18.9 ...
##  $ CRIM   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
##  $ ZN     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
##  $ INDUS  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
##  $ CHAS   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ NOX    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
##  $ RM     : num  6.58 6.42 7.18 7 7.15 ...
##  $ AGE    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
##  $ DIS    : num  4.09 4.97 4.97 6.06 6.06 ...
##  $ RAD    : int  1 2 2 3 3 3 5 5 5 5 ...
##  $ TAX    : int  296 242 242 222 222 222 311 311 311 311 ...
##  $ PTRATIO: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
```

```r

# Summary of polution
summary(boston$NOX)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.385   0.449   0.538   0.555   0.624   0.871
```

```r

# Summary of median value prices
summary(boston$MEDV)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     5.0    17.0    21.2    22.5    25.0    50.0
```



```r
# Plot observations
plot(boston$LON, boston$LAT)

# Tracts alongside the Charles River
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = "blue", 
    pch = 19)

# Plot MIT
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col = "red", 
    pch = 20)

# Plot polution
points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col = "green", 
    pch = 20)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r

# Plot prices
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", 
    pch = 20)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 

```r

# Plot LAT and LON vs. MEDV
plot(boston$LAT, boston$MEDV)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-23.png) 

```r
plot(boston$LON, boston$MEDV)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-24.png) 



```r
latlonlm <- lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)
```

```
## 
## Call:
## lm(formula = MEDV ~ LAT + LON, data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -16.46  -5.59  -1.30   3.69  28.13 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3178.47     484.94   -6.55  1.4e-10 ***
## LAT             8.05       6.33    1.27      0.2    
## LON           -40.27       5.18   -7.77  4.5e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 8.69 on 503 degrees of freedom
## Multiple R-squared: 0.107,	Adjusted R-squared: 0.104 
## F-statistic: 30.2 on 2 and 503 DF,  p-value: 4.16e-13
```

```r
# latlonlm$fitted.values
```



```r
# Visualize regression output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", 
    pch = 20)

points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 
    21.2], col = "blue", pch = "$")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

