MITx: 15.071x The Analytics Edge, Week1 Stock Dynamics
========================================================
# Tarek Dib

## *Introduction*
A stock market is where buyers and sellers trade shares of a company, and is one of the most popular ways for individuals and companies to invest money. The size of the world stock market  is now estimated to be in the trillions. The largest stock market in the world is the New York Stock Exchange (NYSE), located in New York City. About 2,800 companies are listed on the NSYE. In this problem, we'll look at the monthly stock prices of five of these companies: IBM, General Electric (GE), Procter and Gamble, Coca Cola, and Boeing. The data used in this problem comes from Infochimps.

## *Variables*
  
    Date: the date of the stock price, always given as the first of the month.
    StockPrice: the average stock price of the company in the given month.
    
Before working with these data sets, we need to convert the dates into a format that R can understand. Take a look at the structure of one of the datasets using the str function. Right now, the date variable is stored as a factor. Convert this to a "Date" object.

## *Loading Data and Descriptive Statistics*

```r
# Set the directory at where the data is located
setwd("/home/tarek/Analytics/Week1/Rlectures/Data")
# Read the Data
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")
# Convert date factor into a date object
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
# Structure of IBM Stock data
str(IBM)
```

```
## 'data.frame':	480 obs. of  2 variables:
##  $ Date      : Date, format: "1970-01-01" "1970-02-01" ...
##  $ StockPrice: num  360 347 327 320 270 ...
```

```r
max(IBM$Date)
```

```
## [1] "2009-12-01"
```

```r
summary(IBM)
```

```
##       Date              StockPrice   
##  Min.   :1970-01-01   Min.   : 43.4  
##  1st Qu.:1979-12-24   1st Qu.: 88.3  
##  Median :1989-12-16   Median :112.1  
##  Mean   :1989-12-15   Mean   :144.4  
##  3rd Qu.:1999-12-08   3rd Qu.:165.4  
##  Max.   :2009-12-01   Max.   :438.9
```

```r
summary(GE)
```

```
##       Date              StockPrice    
##  Min.   :1970-01-01   Min.   :  9.29  
##  1st Qu.:1979-12-24   1st Qu.: 44.21  
##  Median :1989-12-16   Median : 55.81  
##  Mean   :1989-12-15   Mean   : 59.30  
##  3rd Qu.:1999-12-08   3rd Qu.: 72.23  
##  Max.   :2009-12-01   Max.   :156.84
```

```r
summary(CocaCola)
```

```
##       Date              StockPrice   
##  Min.   :1970-01-01   Min.   : 30.1  
##  1st Qu.:1979-12-24   1st Qu.: 42.8  
##  Median :1989-12-16   Median : 51.4  
##  Mean   :1989-12-15   Mean   : 60.0  
##  3rd Qu.:1999-12-08   3rd Qu.: 69.6  
##  Max.   :2009-12-01   Max.   :146.6
```

```r
summary(Boeing)
```

```
##       Date              StockPrice   
##  Min.   :1970-01-01   Min.   : 12.7  
##  1st Qu.:1979-12-24   1st Qu.: 34.6  
##  Median :1989-12-16   Median : 44.9  
##  Mean   :1989-12-15   Mean   : 46.6  
##  3rd Qu.:1999-12-08   3rd Qu.: 57.2  
##  Max.   :2009-12-01   Max.   :107.3
```

```r
summary(ProcterGamble)
```

```
##       Date              StockPrice   
##  Min.   :1970-01-01   Min.   : 46.9  
##  1st Qu.:1979-12-24   1st Qu.: 62.5  
##  Median :1989-12-16   Median : 78.3  
##  Mean   :1989-12-15   Mean   : 77.7  
##  3rd Qu.:1999-12-08   3rd Qu.: 89.5  
##  Max.   :2009-12-01   Max.   :149.6
```



```r
plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v = as.Date(c("2000-03-01")), lwd = 2)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r

# Visualizing stock dynamics from 1995 to 2005
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type = "l", col = "red", 
    ylim = c(0, 210), xlab = "Year", ylab = "Stock Price", lwd = 2, main = "Stock Price from 1995 to 2005")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = "blue", 
    lwd = 2)
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = "green", lwd = 2)
lines(GE$Date[301:432], GE$StockPrice[301:432], col = "purple", lwd = 2)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = "orange", lwd = 2)
legend("topright", c("Coca Cola", "Procter Gamble", "IBM", "GE", "Boeing"), 
    lty = c(1, 1), lwd = c(2, 2, 2, 2, 2), col = c("red", "blue", "green", "purple", 
        "orange"))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 

```r
# Companies with a decreasing trend in their stock price from September
# 1997 to November 1997. Create vertical lines at September 1997 and
# November 1997 abline(v=as.Date(c('1997-09-01')), lwd=2)
# abline(v=as.Date(c('1997-11-01')), lwd=2)
```



```r
# let's see if stocks tend to be higher or lower during certain months.
tapply(IBM$StockPrice, months(IBM$Date), mean) > mean(IBM$StockPrice)
```

```
##     April    August  December  February   January      July      June 
##      TRUE     FALSE     FALSE      TRUE      TRUE     FALSE     FALSE 
##     March       May  November   October September 
##      TRUE      TRUE     FALSE     FALSE     FALSE
```

```r
tapply(GE$StockPrice, months(GE$Date), mean)
```

```
##     April    August  December  February   January      July      June 
##     64.48     56.50     59.10     62.52     62.05     56.73     56.47 
##     March       May  November   October September 
##     63.15     60.87     57.29     56.24     56.24
```

```r
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
```

```
##     April    August  December  February   January      July      June 
##     62.69     58.88     59.73     60.73     60.37     58.98     60.81 
##     March       May  November   October September 
##     62.07     61.44     59.10     57.94     57.60
```

```r
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
```

```
##     April    August  December  February   January      July      June 
##     47.05     46.86     46.17     46.89     46.51     46.55     47.39 
##     March       May  November   October September 
##     46.88     48.14     45.15     45.22     46.30
```

```r
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
```

```
##     April    August  December  February   January      July      June 
##     77.69     76.82     78.30     79.03     79.62     76.65     77.39 
##     March       May  November   October September 
##     77.35     77.86     78.46     76.68     76.62
```
