MITx: 15.071x The Analytics Edge, Week1, Internet Privacy
========================================================
# Tarek Dib

## *Introduction*
Internet privacy has gained widespread attention in recent years. To measure the degree to which people are concerned about hot-button issues like Internet privacy, social scientists conduct polls in which they interview a large number of people about the topic. In this assignment, we will analyze data from a July 2013 Pew Internet and American Life Project poll on Internet anonymity and privacy, which involved interviews across the United States. While the full polling data can be found here, we will use a more limited version of the results, available in AnonymityPoll.csv.

## *Variables*
The dataset has the following fields (all Internet use-related fields were only collected from interviewees who either use the Internet or have a smartphone):

    Internet.Use: A binary variable indicating if the interviewee uses the Internet, at least occasionally (equals 1 if the interviewee uses the Internet, and equals 0 if the interviewee does not use the Internet).
    Smartphone: A binary variable indicating if the interviewee has a smartphone (equals 1 if they do have a smartphone, and equals 0 if they don't have a smartphone).
    Sex: Male or Female.
    Age: Age in years.
    State: State of residence of the interviewee.
    Region: Census region of the interviewee (Midwest, Northeast, South, or West).
    Conservativeness: Self-described level of conservativeness of interviewee, from 1 (very liberal) to 5 (very conservative).
    Info.On.Internet: Number of the following items this interviewee believes to be available on the Internet for others to see: (1) Their email address; (2) Their home address; (3) Their home phone number; (4) Their cell phone number; (5) The employer/company they work for; (6) Their political party or political affiliation; (7) Things they've written that have their name on it; (8) A photo of them; (9) A video of them; (10) Which groups or organizations they belong to; and (11) Their birth date.
    Worry.About.Info: A binary variable indicating if the interviewee worries about how much information is available about them on the Internet (equals 1 if they worry, and equals 0 if they don't worry).
    Privacy.Importance: A score from 0 (privacy is not too important) to 100 (privacy is very important), which combines the degree to which they find privacy important in the following: (1) The websites they browse; (2) Knowledge of the place they are located when they use the Internet; (3) The content and files they download; (4) The times of day they are online; (5) The applications or programs they use; (6) The searches they perform; (7) The content of their email; (8) The people they exchange email with; and (9) The content of their online chats or hangouts with others.  
    Anonymity.Possible: A binary variable indicating if the interviewee thinks it's possible to use the Internet anonymously, meaning in such a way that online activities can't be traced back to them (equals 1 if he/she believes you can, and equals 0 if he/she believes you can't).
    Anonymity.Possible: A binary variable indicating if the interviewee thinks it's possible to use the Internet anonymously, meaning in such a way that online activities can't be traced back to them (equals 1 if he/she believes you can, and equals 0 if he/she believes you can't).
    Tried.Masking.Identity: A binary variable indicating if the interviewee has ever tried to mask his/her identity when using the Internet (equals 1 if he/she has tried to mask his/her identity, and equals 0 if he/she has not tried to mask his/her identity).
    Privacy.Laws.Effective: A binary variable indicating if the interviewee believes United States law provides reasonable privacy protection for Internet users (equals 1 if he/she believes it does, and equals 0 if he/she believes it doesn't).


## *Loading Data and Descriptive Statistics*

```r
# Set the directory at where the data is located
setwd("/home/tarek/Analytics/Week1/Rlectures/Data")
# Read the Data
poll <- read.csv("AnonymityPoll.csv")
str(poll)
```

```
## 'data.frame':	1002 obs. of  13 variables:
##  $ Internet.Use          : int  1 1 0 1 0 1 1 0 0 1 ...
##  $ Smartphone            : int  0 0 1 0 NA 1 0 0 NA 0 ...
##  $ Sex                   : Factor w/ 2 levels "Female","Male": 2 2 1 2 1 2 1 1 2 1 ...
##  $ Age                   : int  62 45 70 70 80 49 52 76 75 76 ...
##  $ State                 : Factor w/ 49 levels "Alabama","Arizona",..: 20 39 29 10 10 41 21 31 32 32 ...
##  $ Region                : Factor w/ 4 levels "Midwest","Northeast",..: 2 3 2 3 3 3 1 2 3 3 ...
##  $ Conservativeness      : int  4 1 4 4 4 4 3 3 4 4 ...
##  $ Info.On.Internet      : int  0 1 0 3 NA 6 3 NA NA 0 ...
##  $ Worry.About.Info      : int  1 0 0 1 NA 0 1 NA NA 0 ...
##  $ Privacy.Importance    : num  100 0 NA 88.9 NA ...
##  $ Anonymity.Possible    : int  0 1 0 1 NA 1 0 NA NA 1 ...
##  $ Tried.Masking.Identity: int  0 0 0 0 NA 1 0 NA NA 0 ...
##  $ Privacy.Laws.Effective: int  0 1 NA 0 NA 0 1 NA 0 1 ...
```

```r
summary(poll)
```

```
##   Internet.Use     Smartphone       Sex           Age      
##  Min.   :0.000   Min.   :0.00   Female:505   Min.   :18.0  
##  1st Qu.:1.000   1st Qu.:0.00   Male  :497   1st Qu.:37.0  
##  Median :1.000   Median :1.00                Median :55.0  
##  Mean   :0.774   Mean   :0.51                Mean   :52.4  
##  3rd Qu.:1.000   3rd Qu.:1.00                3rd Qu.:66.0  
##  Max.   :1.000   Max.   :1.00                Max.   :96.0  
##  NA's   :1       NA's   :43                  NA's   :27    
##           State           Region    Conservativeness Info.On.Internet
##  California  :103   Midwest  :239   Min.   :1.00     Min.   : 0.0    
##  Texas       : 72   Northeast:166   1st Qu.:3.00     1st Qu.: 2.0    
##  New York    : 60   South    :359   Median :3.00     Median : 4.0    
##  Pennsylvania: 45   West     :238   Mean   :3.28     Mean   : 3.8    
##  Florida     : 42                   3rd Qu.:4.00     3rd Qu.: 6.0    
##  Ohio        : 38                   Max.   :5.00     Max.   :11.0    
##  (Other)     :642                   NA's   :62       NA's   :210     
##  Worry.About.Info Privacy.Importance Anonymity.Possible
##  Min.   :0.00     Min.   :  0.0      Min.   :0.00      
##  1st Qu.:0.00     1st Qu.: 41.4      1st Qu.:0.00      
##  Median :0.00     Median : 68.8      Median :0.00      
##  Mean   :0.49     Mean   : 62.9      Mean   :0.37      
##  3rd Qu.:1.00     3rd Qu.: 88.9      3rd Qu.:1.00      
##  Max.   :1.00     Max.   :100.0      Max.   :1.00      
##  NA's   :212      NA's   :215        NA's   :249       
##  Tried.Masking.Identity Privacy.Laws.Effective
##  Min.   :0.00           Min.   :0.00          
##  1st Qu.:0.00           1st Qu.:0.00          
##  Median :0.00           Median :0.00          
##  Mean   :0.16           Mean   :0.26          
##  3rd Qu.:0.00           3rd Qu.:1.00          
##  Max.   :1.00           Max.   :1.00          
##  NA's   :218            NA's   :108
```

```r
# Summary statistics on smartphone
table(poll$Smartphone)
```

```
## 
##   0   1 
## 472 487
```

```r
summary(poll$Smartphone)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    1.00    0.51    1.00    1.00      43
```

```r

# States in the midwest
MidwestInterviewees = subset(poll, Region == "Midwest")
table(MidwestInterviewees$State)
```

```
## 
##              Alabama              Arizona             Arkansas 
##                    0                    0                    0 
##           California             Colorado          Connecticut 
##                    0                    0                    0 
##             Delaware District of Columbia              Florida 
##                    0                    0                    0 
##              Georgia                Idaho             Illinois 
##                    0                    0                   32 
##              Indiana                 Iowa               Kansas 
##                   27                   14                   14 
##             Kentucky            Louisiana                Maine 
##                    0                    0                    0 
##             Maryland        Massachusetts             Michigan 
##                    0                    0                   31 
##            Minnesota          Mississippi             Missouri 
##                   15                    0                   26 
##              Montana             Nebraska               Nevada 
##                    0                   11                    0 
##        New Hampshire           New Jersey           New Mexico 
##                    0                    0                    0 
##             New York       North Carolina         North Dakota 
##                    0                    0                    5 
##                 Ohio             Oklahoma               Oregon 
##                   38                    0                    0 
##         Pennsylvania         Rhode Island       South Carolina 
##                    0                    0                    0 
##         South Dakota            Tennessee                Texas 
##                    3                    0                    0 
##                 Utah              Vermont             Virginia 
##                    0                    0                    0 
##           Washington        West Virginia            Wisconsin 
##                    0                    0                   23 
##              Wyoming 
##                    0
```

```r

# Interviewees from each South region state
SouthInterviewees = subset(poll, Region == "South")
table(SouthInterviewees$State)
```

```
## 
##              Alabama              Arizona             Arkansas 
##                   11                    0                   10 
##           California             Colorado          Connecticut 
##                    0                    0                    0 
##             Delaware District of Columbia              Florida 
##                    6                    2                   42 
##              Georgia                Idaho             Illinois 
##                   34                    0                    0 
##              Indiana                 Iowa               Kansas 
##                    0                    0                    0 
##             Kentucky            Louisiana                Maine 
##                   25                   17                    0 
##             Maryland        Massachusetts             Michigan 
##                   18                    0                    0 
##            Minnesota          Mississippi             Missouri 
##                    0                   11                    0 
##              Montana             Nebraska               Nevada 
##                    0                    0                    0 
##        New Hampshire           New Jersey           New Mexico 
##                    0                    0                    0 
##             New York       North Carolina         North Dakota 
##                    0                   32                    0 
##                 Ohio             Oklahoma               Oregon 
##                    0                   14                    0 
##         Pennsylvania         Rhode Island       South Carolina 
##                    0                    0                   12 
##         South Dakota            Tennessee                Texas 
##                    0                   17                   72 
##                 Utah              Vermont             Virginia 
##                    0                    0                   31 
##           Washington        West Virginia            Wisconsin 
##                    0                    5                    0 
##              Wyoming 
##                    0
```

```r
# Summary table of smartphone and internet uses
table(poll$Internet.Use, poll$Smartphone)
```

```
##    
##       0   1
##   0 186  17
##   1 285 470
```

```r
# limit to interviewees who reported Internet use or who reported
# smartphone use.
limited = subset(poll, Internet.Use == 1 | Smartphone == 1)
summary(limited)
```

```
##   Internet.Use     Smartphone        Sex           Age      
##  Min.   :0.000   Min.   :0.000   Female:392   Min.   :18.0  
##  1st Qu.:1.000   1st Qu.:0.000   Male  :400   1st Qu.:33.0  
##  Median :1.000   Median :1.000                Median :51.0  
##  Mean   :0.979   Mean   :0.631                Mean   :48.6  
##  3rd Qu.:1.000   3rd Qu.:1.000                3rd Qu.:62.0  
##  Max.   :1.000   Max.   :1.000                Max.   :93.0  
##                  NA's   :20                   NA's   :22    
##             State           Region    Conservativeness Info.On.Internet
##  California    : 89   Midwest  :172   Min.   :1.00     Min.   : 0.00   
##  Texas         : 57   Northeast:128   1st Qu.:3.00     1st Qu.: 2.00   
##  New York      : 45   South    :288   Median :3.00     Median : 4.00   
##  Pennsylvania  : 33   West     :204   Mean   :3.24     Mean   : 3.79   
##  Florida       : 32                   3rd Qu.:4.00     3rd Qu.: 6.00   
##  North Carolina: 28                   Max.   :5.00     Max.   :11.00   
##  (Other)       :508                   NA's   :45                       
##  Worry.About.Info Privacy.Importance Anonymity.Possible
##  Min.   :0.000    Min.   :  0.0      Min.   :0.00      
##  1st Qu.:0.000    1st Qu.: 41.4      1st Qu.:0.00      
##  Median :0.000    Median : 68.8      Median :0.00      
##  Mean   :0.489    Mean   : 62.9      Mean   :0.37      
##  3rd Qu.:1.000    3rd Qu.: 88.9      3rd Qu.:1.00      
##  Max.   :1.000    Max.   :100.0      Max.   :1.00      
##  NA's   :2        NA's   :5          NA's   :39        
##  Tried.Masking.Identity Privacy.Laws.Effective
##  Min.   :0.000          Min.   :0.00          
##  1st Qu.:0.000          1st Qu.:0.00          
##  Median :0.000          Median :0.00          
##  Mean   :0.163          Mean   :0.26          
##  3rd Qu.:0.000          3rd Qu.:1.00          
##  Max.   :1.000          Max.   :1.00          
##  NA's   :8              NA's   :65
```

```r
mean(limited$Info.On.Internet)
```

```
## [1] 3.795
```

```r
table(limited$Info.On.Internet)
```

```
## 
##   0   1   2   3   4   5   6   7   8   9  10  11 
## 105  84  95 101 104  94  67  63  40  18  13   8
```

```r
# Proportion of interviewees who answered the Worry.About.Info question
# worry about how much information is available about them on the Internet
sum(limited$Worry.About.Info == 1, na.rm = T)/sum(limited$Worry.About.Info == 
    1 | limited$Worry.About.Info == 0, na.rm = T)
```

```
## [1] 0.4886
```

```r
# proportion of interviewees who answered the Anonymity.Possible question
# who think it is possible to be completely anonymous on the Internet
table(limited$Anonymity.Possible)
```

```
## 
##   0   1 
## 475 278
```

```r
278/(475 + 278)
```

```
## [1] 0.3692
```

```r
# proportion of interviewees who answered the Tried.Masking.Identity
# question have tried masking their identity on the Internet
table(limited$Tried.Masking.Identity)
```

```
## 
##   0   1 
## 656 128
```

```r
128/(656 + 128)
```

```
## [1] 0.1633
```

```r
# proportion of interviewees who answered the Privacy.Laws.Effective
# question find United States privacy laws effective
table(limited$Privacy.Laws.Effective)
```

```
## 
##   0   1 
## 541 186
```

```r
186/(541 + 186)
```

```
## [1] 0.2558
```

```r
# largest number of interviewees that have exactly the same value in their
# Age variable AND the same value in their Info.On.Internet variable
max(table(limited$Age, limited$Info.On.Internet))
```

```
## [1] 6
```

```r
# Use the tapply() function to obtain the summary of the Info.On.Internet
# value, broken down by whether an interviewee is a smartphone user.
tapply(limited$Info.On.Internet, limited$Smartphone, mean)
```

```
##     0     1 
## 2.923 4.368
```

```r
# proportion of smartphone users who answered the Tried.Masking.Identity
# question have tried masking their identity when using the Internet. And
# non-smartphone users
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
```

```
## $`0`
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   0.000   0.117   0.000   1.000       4 
## 
## $`1`
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   0.000   0.193   0.000   1.000       4
```



```r
hist(limited$Age, breaks = 50)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 


