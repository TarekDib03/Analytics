MITx: 15.071x The Analytics Edge
------------------------------------------------------------------
# Predicting the Baseball World Series Champion
#### Tarek Dib
#### Date: March 24, 2014

### *Introduction*

Variables in the data set:

    Team: A code for the name of the team
    League: The Major League Baseball league the team belongs to, either AL (American League) or NL (National League)
    Year: The year of the corresponding record
    RS: The number of runs scored by the team in that year
    RA: The number of runs allowed by the team in that year
    W: The number of regular season wins by the team in that year
    OBP: The on-base percentage of the team in that year
    SLG: The slugging percentage of the team in that year
    BA: The batting average of the team in that year
    Playoffs: Whether the team made the playoffs in that year (1 for yes, 0 for no)
    RankSeason: Among the playoff teams in that year, the ranking of their regular season records (1 is best)
    RankPlayoffs: Among the playoff teams in that year, how well they fared in the playoffs. The team winning the World Series gets a RankPlayoffs of 1.
    G: The number of games a team played in that year
    OOBP: The team's opponents' on-base percentage in that year
    OSLG: The team's opponents' slugging percentage in that year


## *Limiting to Teams Making the Playoffs*

```r
# Read the data
baseball <- read.csv("baseball.csv")

# Structure of the data set
str(baseball)
```

```
## 'data.frame':	1232 obs. of  15 variables:
##  $ Team        : Factor w/ 39 levels "ANA","ARI","ATL",..: 2 3 4 5 7 8 9 10 11 12 ...
##  $ League      : Factor w/ 2 levels "AL","NL": 2 2 1 1 2 1 2 1 2 1 ...
##  $ Year        : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
##  $ RS          : int  734 700 712 734 613 748 669 667 758 726 ...
##  $ RA          : int  688 600 705 806 759 676 588 845 890 670 ...
##  $ W           : int  81 94 93 69 61 85 97 68 64 88 ...
##  $ OBP         : num  0.328 0.32 0.311 0.315 0.302 0.318 0.315 0.324 0.33 0.335 ...
##  $ SLG         : num  0.418 0.389 0.417 0.415 0.378 0.422 0.411 0.381 0.436 0.422 ...
##  $ BA          : num  0.259 0.247 0.247 0.26 0.24 0.255 0.251 0.251 0.274 0.268 ...
##  $ Playoffs    : int  0 1 1 0 0 0 1 0 0 1 ...
##  $ RankSeason  : int  NA 4 5 NA NA NA 2 NA NA 6 ...
##  $ RankPlayoffs: int  NA 5 4 NA NA NA 4 NA NA 2 ...
##  $ G           : int  162 162 162 162 162 162 162 162 162 162 ...
##  $ OOBP        : num  0.317 0.306 0.315 0.331 0.335 0.319 0.305 0.336 0.357 0.314 ...
##  $ OSLG        : num  0.415 0.378 0.403 0.428 0.424 0.405 0.39 0.43 0.47 0.402 ...
```

```r

# Total Number of years included in the data
length(table(baseball$Year))
```

```
## [1] 47
```

```r

# Teams that made it to the playoff
baseball <- subset(baseball, baseball$Playoffs == 1)

# The number of teams making the playoffs from 1962 to 2012
table(baseball$Year)
```

```
## 
## 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1973 1974 1975 1976 1977 
##    2    2    2    2    2    2    2    4    4    4    4    4    4    4    4 
## 1978 1979 1980 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 
##    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4 
## 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2011 2012 
##    8   10
```


## *Adding an Important Predictor*
It's much harder to win the World Series if there are 10 teams competing for the championship versus just two. Therefore, we will add the predictor variable NumCompetitors to the baseball data frame. NumCompetitors will contain the number of total teams making the playoffs in the year of a particular team/year pair. For instance, NumCompetitors should be 2 for the 1962 New York Yankees, but it should be 8 for the 1998 Boston Red Sox.

```r
PlayoffTable = table(baseball$Year)
names(PlayoffTable)
```

```
##  [1] "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970" "1971"
## [11] "1973" "1974" "1975" "1976" "1977" "1978" "1979" "1980" "1982" "1983"
## [21] "1984" "1985" "1986" "1987" "1988" "1989" "1990" "1991" "1992" "1993"
## [31] "1996" "1997" "1998" "1999" "2000" "2001" "2002" "2003" "2004" "2005"
## [41] "2006" "2007" "2008" "2009" "2010" "2011" "2012"
```

```r

# Because PlayoffTable is an object and not a function, we look up
# elements in it with square brackets instead of parentheses.
PlayoffTable[c("1990", "2010")]
```

```
## 
## 1990 2010 
##    4    8
```

```r

# Look up the number of teams in the playoffs for each team/year pair in
# the dataset, and store it as a new variable named NumCompetitors in the
# baseball data frame.
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

# Obtain the number of team/year pairs with 8 teams in the playoffs
table(baseball$NumCompetitors)
```

```
## 
##   2   4   8  10 
##  14  92 128  10
```

```r
# Or
baseball.8 <- subset(baseball, baseball$NumCompetitors == 8)
sum(table(baseball.8$Team))
```

```
## [1] 128
```


## *Bivariate Models for Predicting World Series Winner*
In this problem, we seek to predict whether a team won the World Series; in our dataset this is denoted with a RankPlayoffs value of 1

```r
# Add a variable named WorldSeries to the baseball data frame
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
# Number of teams who won the World Series
length(baseball$WorldSeries[baseball$WorldSeries == 0])
```

```
## [1] 197
```

```r

# Build the model
LogModel = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, 
    family = binomial)
summary(LogModel)
```

```
## 
## Call:
## glm(formula = WorldSeries ~ Year + RA + RankSeason + NumCompetitors, 
##     family = binomial, data = baseball)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.034  -0.769  -0.514  -0.458   2.220  
## 
## Coefficients:
##                 Estimate Std. Error z value Pr(>|z|)
## (Intercept)    12.587438  53.647421    0.23     0.81
## Year           -0.006142   0.027466   -0.22     0.82
## RA             -0.000824   0.002739   -0.30     0.76
## RankSeason     -0.068505   0.120346   -0.57     0.57
## NumCompetitors -0.179426   0.181593   -0.99     0.32
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 226.37  on 239  degrees of freedom
## AIC: 236.4
## 
## Number of Fisher Scoring iterations: 4
```

```r
# Find the correlation matrix of the variables in the model
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])
```

```
##                  Year     RA RankSeason NumCompetitors
## Year           1.0000 0.4762     0.3852         0.9140
## RA             0.4762 1.0000     0.3991         0.5137
## RankSeason     0.3852 0.3991     1.0000         0.4247
## NumCompetitors 0.9140 0.5137     0.4247         1.0000
```

```r

# Model selection
library(MASS)
stepAIC(LogModel, direction = "backward")
```

```
## Start:  AIC=236.4
## WorldSeries ~ Year + RA + RankSeason + NumCompetitors
## 
##                  Df Deviance AIC
## - Year            1      226 234
## - RA              1      226 234
## - RankSeason      1      227 235
## - NumCompetitors  1      227 235
## <none>                   226 236
## 
## Step:  AIC=234.4
## WorldSeries ~ RA + RankSeason + NumCompetitors
## 
##                  Df Deviance AIC
## - RA              1      226 232
## - RankSeason      1      227 233
## <none>                   226 234
## - NumCompetitors  1      232 238
## 
## Step:  AIC=232.5
## WorldSeries ~ RankSeason + NumCompetitors
## 
##                  Df Deviance AIC
## - RankSeason      1      227 231
## <none>                   226 232
## - NumCompetitors  1      235 239
## 
## Step:  AIC=231
## WorldSeries ~ NumCompetitors
## 
##                  Df Deviance AIC
## <none>                   227 231
## - NumCompetitors  1      239 241
```

```
## 
## Call:  glm(formula = WorldSeries ~ NumCompetitors, family = binomial, 
##     data = baseball)
## 
## Coefficients:
##    (Intercept)  NumCompetitors  
##         0.0387         -0.2522  
## 
## Degrees of Freedom: 243 Total (i.e. Null);  242 Residual
## Null Deviance:	    239 
## Residual Deviance: 227 	AIC: 231
```

