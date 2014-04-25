Data Analysis and Statistical Inference Project 
========================================================
# Tarek Dib

## *Introduction*
Crime is an international concern, but it is documented and handled in very different ways in different countries. In the United States, violent crimes and property crimes are recorded by the Federal Bureau of Investigation (FBI).  Additionally, each city documents crime, and some cities release data regarding crime rates. The city of Chicag, Illinois releases crime data from 2001 onward online. Chicago is the third most populous city in the United States, with a population of over 2.7 million people.

There are two main types of crimes: violent crimes, and property crimes. In this proble, we'll focus on one specific type of property crime, called "motor vehicle theft" (sometimes referred to as grand theft auto). This is the act of stealing, or attempting to steal, a car. In this problem, we'll use some basic data analysis in R to understand the motor vehicle thefts in Chicago. 

## *Variables*
    ID: a unique identifier for each observation
    Date: the date the crime occurred
    LocationDescription: the location where the crime occurred
    Arrest: whether or not an arrest was made for the crime (TRUE if an arrest was made, and FALSE if an arrest was not made)
    Domestic: whether or not the crime was a domestic crime, meaning that it was committed against a family member (TRUE if it was domestic, and FALSE if it was not domestic)
    Beat: the area, or "beat" in which the crime occurred. This is the smallest regional division defined by the Chicago police department.
    District: the police district in which the crime occured. Each district is composed of many beats, and are defined by the Chicago Police Department.
    CommunityArea: the community area in which the crime occurred. Since the 1920s, Chicago has been divided into what are called "community areas", of which there are now 77. The community areas were devised in an attempt to create socially homogeneous regions.
    Year: the year in which the crime occurred.
    Latitude: the latitude of the location at which the crime occurred.
    Longitude: the longitude of the location at which the crime occurred.

## *Loading Data and Descriptive Statistics*

```r
# Set the directory at where the data is located
setwd("/home/tarek/Analytics/Week1/Rlectures/Data")
# Read the Data
mvt <- read.csv("mvt.csv")
# Structure of data
str(mvt)
```

```
## 'data.frame':	191641 obs. of  11 variables:
##  $ ID                 : int  8951354 8951141 8952745 8952223 8951608 8950793 8950760 8951611 8951802 8950706 ...
##  $ Date               : Factor w/ 131680 levels "10/10/01 0:00",..: 37302 37300 37300 37300 37299 37297 37296 37295 37291 37290 ...
##  $ LocationDescription: Factor w/ 78 levels "ABANDONED BUILDING",..: 72 72 62 72 72 72 72 72 72 72 ...
##  $ Arrest             : logi  FALSE FALSE FALSE FALSE FALSE TRUE ...
##  $ Domestic           : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ Beat               : int  623 1213 1622 724 211 2521 423 231 1021 1215 ...
##  $ District           : int  6 12 16 7 2 25 4 2 10 12 ...
##  $ CommunityArea      : int  69 24 11 67 35 19 48 40 29 24 ...
##  $ Year               : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
##  $ Latitude           : num  41.8 41.9 42 41.8 41.8 ...
##  $ Longitude          : num  -87.6 -87.7 -87.8 -87.7 -87.6 ...
```

```r
# number of crimes for which an arrest was made
sum(mvt$Arrest == "TRUE")
```

```
## [1] 15536
```

```r
# Convert date characters into a Date object in R
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
```

```
##         Min.      1st Qu.       Median         Mean      3rd Qu. 
## "2001-01-01" "2003-07-10" "2006-05-21" "2006-08-23" "2009-10-24" 
##         Max. 
## "2012-12-31"
```

```r
# extract the month and the day of the week, and add these variables to
# our data frame mvt
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
# Replace the old Date variable with DateConvert
mvt$Date = DateConvert
# month that had the fewest motor vehicle thefts
which.min(table(mvt$Month))
```

```
## February 
##        4
```

```r
# weekday that had the most motor vehicle thefts
which.max(table(mvt$Weekday))
```

```
## Friday 
##      1
```

```r
# month that has the largest number of motor vehicle thefts for which an
# arrest was made
which.max(table(mvt$Arrest == "TRUE", mvt$Month)[2, ])
```

```
## January 
##       5
```

```r
table(mvt$Month)
```

```
## 
##     April    August  December  February   January      July      June 
##     15280     16572     16426     13511     16047     16801     16002 
##     March       May  November   October September 
##     15758     16035     16063     17086     16060
```

```r
# Proportion of motor vehicle thefts in 2001 in which an arrest was made
sum(mvt$Year == 2001 & mvt$Arrest == "TRUE")/sum(mvt$Year == 2001)
```

```
## [1] 0.1041
```

```r
# Arrests made in 2007 and 2012
sum(mvt$Year == 2007 & mvt$Arrest == "TRUE")/sum(mvt$Year == 2007)
```

```
## [1] 0.08487
```

```r
sum(mvt$Year == 2012 & mvt$Arrest == "TRUE")/sum(mvt$Year == 2012)
```

```
## [1] 0.03903
```

```r
write.table(mvt, "mvtData.csv")
```


*Popular Locations*
--------------------------------------------------------------------------
Analyzing this data could be useful to the Chicago Police Department when deciding where to allocate resources. If they want to increase the number of arrests that are made for motor vehicle thefts, where should they focus their efforts?

```r
sort(table(mvt$LocationDescription))[73:78]
```

```
## 
##         DRIVEWAY - RESIDENTIAL                    GAS STATION 
##                           1675                           2111 
##                          ALLEY                          OTHER 
##                           2308                           4573 
## PARKING LOT/GARAGE(NON.RESID.)                         STREET 
##                          14852                         156564
```

```r
# Create a subset of data, only taking observations for which the theft
# happened in one of the top 5 locations
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", 
    "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)
str(Top5)
```

```
## 'data.frame':	177510 obs. of  13 variables:
##  $ ID                 : int  8951354 8951141 8952223 8951608 8950793 8950760 8951611 8951802 8950706 8951585 ...
##  $ Date               : Date, format: "2012-12-31" "2012-12-31" ...
##  $ LocationDescription: Factor w/ 78 levels "ABANDONED BUILDING",..: 72 72 72 72 72 72 72 72 72 72 ...
##  $ Arrest             : logi  FALSE FALSE FALSE FALSE TRUE FALSE ...
##  $ Domestic           : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ Beat               : int  623 1213 724 211 2521 423 231 1021 1215 1011 ...
##  $ District           : int  6 12 7 2 25 4 2 10 12 10 ...
##  $ CommunityArea      : int  69 24 67 35 19 48 40 29 24 29 ...
##  $ Year               : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
##  $ Latitude           : num  41.8 41.9 41.8 41.8 41.9 ...
##  $ Longitude          : num  -87.6 -87.7 -87.7 -87.6 -87.8 ...
##  $ Month              : chr  "December" "December" "December" "December" ...
##  $ Weekday            : chr  "Monday" "Monday" "Monday" "Monday" ...
```

```r
# Refresh data for Top5
Top5$LocationDescription = factor(Top5$LocationDescription)
t = table(Top5$LocationDescription)
df1 <- as.data.frame(table(Top5$LocationDescription, Top5$Arrest))
df2 <- rep(as.data.frame(t)[, 2], 2)
df <- data.frame(df1, df2)
df$percentage <- round(df$Freq/df$df2, 3) * 100
colnames(df)[1] = "Location"
colnames(df)[2] = "Arrest"
colnames(df)[3] = "Arrests"
colnames(df)[4] = "Thefts"
# Subset of a data in which an arrest was made
T = df[df$Arrest == TRUE, ]
# Sort the subset data in descending order of percentage to find the
# location at which the highest percentage of arrests were made
T[order(-T$percentage), ][1, ]
```

```
##      Location Arrest Arrests Thefts percentage
## 8 GAS STATION   TRUE     439   2111       20.8
```

```r
# Day of the week in which the most motor vehicle thefts at gas stations
# happen
table(Top5$LocationDescription, Top5$Weekday)
```

```
##                                 
##                                  Friday Monday Saturday Sunday Thursday
##   ALLEY                             385    320      341    307      315
##   DRIVEWAY - RESIDENTIAL            257    255      202    221      263
##   GAS STATION                       332    280      338    336      282
##   PARKING LOT/GARAGE(NON.RESID.)   2331   2128     2199   1936     2082
##   STREET                          23773  22305    22175  21756    22296
##                                 
##                                  Tuesday Wednesday
##   ALLEY                              323       317
##   DRIVEWAY - RESIDENTIAL             243       234
##   GAS STATION                        270       273
##   PARKING LOT/GARAGE(NON.RESID.)    2073      2103
##   STREET                           21888     22371
```

```r
levels(Top5$LocationDescription)
```

```
## [1] "ALLEY"                          "DRIVEWAY - RESIDENTIAL"        
## [3] "GAS STATION"                    "PARKING LOT/GARAGE(NON.RESID.)"
## [5] "STREET"
```


## *Exploratory Data Analysis*

```r
# Distribution of crime. Study the trend to understand how crime changed
# over time
hist(mvt$Date, breaks = 100, ylab = "Monthly Number of Motor Vehicle Theft")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



```r
# Distribution of crime. Study the trend to understand how crime changed
# over time
boxplot(mvt$Date ~ mvt$Arrest)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

