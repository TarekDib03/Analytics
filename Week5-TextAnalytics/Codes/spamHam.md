MITx: 15.071x The Analytics Edge - SEPARATING SPAM FROM HAM
========================================================

## *Introduction*
Nearly every email user has at some point encountered a "spam" email, which is an unsolicited message often advertising a product, containing links to malware, or attempting to scam the recipient. Roughly 80-90% of more than 100 billion emails sent each day are spam emails, most being sent from botnets of malware-infected computers. The remainder of emails are called "ham" emails.

As a result of the huge number of spam emails being sent across the Internet each day, most email providers offer a spam filter that automatically flags likely spam messages and separates them from the ham. Though these filters use a number of techniques (e.g. looking up the sender in a so-called "Blackhole List" that contains IP addresses of likely spammers), most rely heavily on the analysis of the contents of an email via text analytics.

In this homework problem, we will build and evaluate a spam filter using a publicly available dataset first described in the 2006 conference paper "Spam Filtering with Naive Bayes -- Which Naive Bayes?" by V. Metsis, I. Androutsopoulos, and G. Paliouras. The "ham" messages in this dataset come from the inbox of former Enron Managing Director for Research Vincent Kaminski, one of the inboxes in the Enron Corpus. One source of spam messages in this dataset is the SpamAssassin corpus, which contains hand-labeled spam messages contributed by Internet users. The remaining spam was collected by Project Honey Pot, a project that collects spam messages and identifies spammers by publishing email address that humans would know not to contact but that bots might target with spam. The full dataset we will use was constructed as roughly a 75/25 mix of the ham and spam messages.

The dataset contains just two fields:

    text: The text of the email.
    spam: A binary variable indicating if the email was spam.

## *Understanding the Data*

```r
# Data
setwd("/home/tarek/Analytics/Weeks/Week5-TextAnalytics/Data")
emails <- read.csv("emails.csv", stringsAsFactors = F)
str(emails)
```

```
## 'data.frame':	5728 obs. of  2 variables:
##  $ text: chr  "Subject: naturally irresistible your corporate identity  lt is really hard to recollect a company : the  market is full of suqg"| __truncated__ "Subject: the stock trading gunslinger  fanny is merrill but muzo not colza attainder and penultimate like esmark perspicuous ra"| __truncated__ "Subject: unbelievable new homes made easy  im wanting to show you this  homeowner  you have been pre - approved for a $ 454 , 1"| __truncated__ "Subject: 4 color printing special  request additional information now ! click here  click here for a printable version of our o"| __truncated__ ...
##  $ spam: int  1 1 1 1 1 1 1 1 1 1 ...
```

```r

# Number of spam emails
sum(emails$spam == 1)
```

```
## [1] 1368
```

```r

# How many characters are in the longest email?
max(nchar(emails$text))
```

```
## [1] 43952
```

```r

# Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))
```

```
## [1] 1992
```


## *Preparing the Corpus*

```r
# Pre process data
library(tm)
# Create Corpus
corpus <- Corpus(VectorSource(emails$text))

# Convert to lower case
corpus <- tm_map(corpus, tolower)

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove Stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Stem the words
corpus <- tm_map(corpus, stemDocument)

# Create matrix
dtm <- DocumentTermMatrix(corpus)
dtm
```

```
## A document-term matrix (5728 documents, 28687 terms)
## 
## Non-/sparse entries: 481719/163837417
## Sparsity           : 100%
## Maximal term length: 24 
## Weighting          : term frequency (tf)
```

```r

# Filter out sparse terms by keeping only terms that appear in at least 5%
# or more of the documents
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm
```

```
## A document-term matrix (5728 documents, 330 terms)
## 
## Non-/sparse entries: 213551/1676689
## Sparsity           : 89%
## Maximal term length: 10 
## Weighting          : term frequency (tf)
```

```r

# Convert spdtm and to a data frame
emailsSparse <- as.data.frame(as.matrix(spdtm))  # make.names is set to true to make the variable names of emailsSparse valid
colnames(emailsSparse) <- make.names(colnames(emailsSparse), unique = T)

# What is the word stem that shows up most frequently across all the emails
# in the dataset?
which.max(colSums(emailsSparse))
```

```
## enron 
##    92
```

```r

# Add variable spam
emailsSparse$spam <- emails$spam

# Create a data set where spam == 0 (ham). The ham dataset is certainly
# personalized to Vincent Kaminski, and therefore it might not generalize
# well to a general email user. Caution is definitely necessary before
# applying the filters derived in this problem to other email users.
ham <- emailsSparse[emailsSparse$spam == 0, ]
# How many word stems appear at least 5000 times in the ham emails in the
# dataset?
sum(colSums(ham) >= 5000)
```

```
## [1] 6
```

```r

# Spam data set
sort(colSums(subset(emailsSparse, spam == 1)))
```

```
##       X713   crenshaw      enron     gibner   kaminski    stinson 
##          0          0          0          0          0          0 
##     vkamin       X853       vinc        doc      kevin    shirley 
##          0          1          1          2          2          2 
##      deriv      april    houston      resum        edu     friday 
##          3          5          5          5          7          7 
##        hou  wednesday        ect     arrang  interview     attend 
##          8          8         10         11         13         15 
##     london     robert    student    schedul   thursday     monday 
##         15         16         16         17         17         19 
##       john    tuesday     attach    suggest    appreci       mark 
##         20         20         21         21         23         25 
##      begin    comment    analysi      X2001      model       hope 
##         26         26         27         29         29         30 
##    mention      X2000     togeth     confer      invit    univers 
##         30         32         32         33         33         34 
##     financ       talk     either        run       morn      shall 
##         35         38         39         39         40         40 
##      happi    thought     depart    confirm    respond     school 
##         42         42         46         47         48         48 
##       corp        etc       hear      howev      sorri       idea 
##         49         49         49         49         50         51 
##     energi    discuss       open     option       soon understand 
##         55         56         56         56         57         57 
##      cours     experi     associ      point      bring   director 
##         59         59         62         62         63         65 
##   particip      anoth       join      still      final   research 
##         65         66         66         66         68         68 
##       case        set     specif      given       juli    problem 
##         69         69         69         70         71         73 
##        put    alreadi        ask        abl       deal        fax 
##         73         74         74         75         75         75 
##       book       team       issu      locat       meet      updat 
##         76         76         79         79         79         79 
##        lot     sincer     better      short       sinc       done 
##         80         80         82         82         82         83 
##   question     recent    possibl   contract        end       move 
##         83         83         84         85         85         86 
##       data      might    continu       note       feel    resourc 
##         87         87         88         88         90         90 
##      sever       area   communic     realli        due     direct 
##         90         92         92         93         94         96 
##     origin       copi       unit       long     member       sure 
##         96         97         97         98         99         99 
##      allow       dear     public      write      event        let 
##        102        104        104        104        105        107 
##     differ       file     involv    respons      creat       type 
##        109        111        111        113        114        114 
##     approv     detail     effort     intern    request        say 
##        115        115        115        117        117        118 
##     import    support       part      relat     assist       last 
##        119        120        121        121        123        124 
##        two       back       keep      addit       date      place 
##        124        125        125        126        127        128 
##      group       mean       valu      think      offic       read 
##        130        131        131        132        133        134 
##     immedi      check     applic      hello        tri     review 
##        136        137        139        139        140        142 
##     believ      phone       hour      power    present    process 
##        143        143        144        145        146        149 
##     corpor       oper       full     return       come       sent 
##        151        151        152        154        155        155 
##   opportun       real      repli       line      engin       term 
##        158        158        158        159        160        161 
##     credit       well        gas       info       plan      next. 
##        162        164        165        165        166        170 
##       risk    increas     access       give      thank       link 
##        170        171        172        172        172        174 
##     requir    version       cost      great       wish     regard 
##        174        174        175        182        185        186 
##      posit      thing       call    develop    complet       much 
##        187        188        190        191        192        192 
##       even    project     design       form     expect     person 
##        193        194        196        196        198        198 
##    without        buy      trade     effect       rate       base 
##        198        199        199        201        201        202 
##       find    current      first      chang      visit    financi 
##        202        203        203        204        206        207 
##       high       mani    forward       good    special        don 
##        208        208        209        221        225        226 
##    success        per     number       week     result        web 
##        226        230        231        231        237        238 
##   industri    contact       made     follow      month      right 
##        239        242        242        244        249        249 
##      today       also       help   internet      manag       know 
##        251        260        262        262        266        269 
##        way      avail      state      futur       home      start 
##        278        280        280        282        285        300 
##     system       take        net     includ       life        see 
##        302        304        305        314        320        329 
##       name      onlin     within      remov       best    program 
##        344        345        346        357        358        358 
##      peopl     custom       year       like   interest       send 
##        359        363        367        372        385        393 
##     servic       look       work        day       want    product 
##        395        396        415        420        420        421 
##        www    account     provid       need    softwar     messag 
##        426        428        435        438        440        445 
##       site    address        may       list      price        new 
##        455        461        489        503        503        504 
##     websit     report      secur       just      offer     invest 
##        506        507        520        524        528        540 
##      order        use      click       X000        now        one 
##        541        546        552        560        575        592 
##       time       http     market       make       free      pleas 
##        593        600        600        603        606        619 
##      money        get     receiv     inform        can      email 
##        662        694        727        818        831        865 
##       busi       mail        com    compani       spam       will 
##        897        917        999       1065       1368       1450 
##    subject 
##       1577
```


## *BUILDING MACHINE LEARNING MODELS*
First, convert the dependent variable to a factor with "emailsSparse$spam = as.factor(emailsSparse$spam)".

Next, set the random seed to 123 and use the sample.split function to split emailsSparse 70/30 into a training set called "train" and a testing set called "test". Make sure to perform this step on emailsSparse instead of emails.

Using the training set, train the following three machine learning models. The models should predict the dependent variable "spam", using all other available variables as independent variables.

1) A logistic regression model called spamLog.

2) A CART model called spamCART, using the default parameters to train the model.

3) A random forest model called spamRF, using the default parameters to train the model. Directly before training the random forest model, set the random seed to 123.

For each model, obtain the predicted spam probabilities for the training set. Be careful to obtain probabilities instead of predicted classes, because we will be using these values to compute training set AUC values. Recall that you can obtain probabilities for CART models by not passing any type parameter to the predict() function, and you can obtain probabilities from a random forest by adding the argument type="prob". For CART and random forest, you need to select the second column of the output of the predict() function, corresponding to the probability of a message being spam.

You may have noticed that training the logistic regression model yielded the messages "algorithm did not converge" and "fitted probabilities numerically 0 or 1 occurred". Both of these messages often indicate overfitting and the first indicates particularly severe overfitting, often to the point that the training set observations are fit perfectly by the model. Let's investigate the predicted probabilities from the logistic regression model.

```r
emailsSparse$spam <- as.factor(emailsSparse$spam)

# Load CaTools
library(caTools)
set.seed(123)
spl <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, spl == T)
test <- subset(emailsSparse, spl == F)

# Logistic regression model
spamLog <- glm(spam ~ ., data = train, family = binomial)
```

```
## Warning: glm.fit: algorithm did not converge
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
summary(spamLog)
```

```
## 
## Call:
## glm(formula = spam ~ ., family = binomial, data = train)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -1.01    0.00    0.00    0.00    1.35  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept) -3.08e+01   1.05e+04    0.00     1.00
## X000         1.47e+01   1.06e+04    0.00     1.00
## X2000       -3.63e+01   1.56e+04    0.00     1.00
## X2001       -3.21e+01   1.32e+04    0.00     1.00
## X713        -2.43e+01   2.91e+04    0.00     1.00
## X853        -1.21e+00   5.94e+04    0.00     1.00
## abl         -2.05e+00   2.09e+04    0.00     1.00
## access      -1.48e+01   1.34e+04    0.00     1.00
## account      2.49e+01   8.16e+03    0.00     1.00
## addit        1.46e+00   2.70e+04    0.00     1.00
## address     -4.61e+00   1.11e+04    0.00     1.00
## allow        1.90e+01   6.44e+03    0.00     1.00
## alreadi     -2.41e+01   3.32e+04    0.00     1.00
## also         2.99e+01   1.38e+04    0.00     1.00
## analysi     -2.41e+01   3.86e+04    0.00     1.00
## anoth       -8.74e+00   2.03e+04    0.00     1.00
## applic      -2.65e+00   1.67e+04    0.00     1.00
## appreci     -2.14e+01   2.76e+04    0.00     1.00
## approv      -1.30e+00   1.59e+04    0.00     1.00
## april       -2.62e+01   2.21e+04    0.00     1.00
## area         2.04e+01   2.27e+04    0.00     1.00
## arrang       1.07e+01   2.14e+04    0.00     1.00
## ask         -7.75e+00   1.98e+04    0.00     1.00
## assist      -1.13e+01   2.49e+04    0.00     1.00
## associ       9.05e+00   1.91e+04    0.00     1.00
## attach      -1.04e+01   1.53e+04    0.00     1.00
## attend      -3.45e+01   3.26e+04    0.00     1.00
## avail        8.65e+00   1.71e+04    0.00     1.00
## back        -1.32e+01   2.27e+04    0.00     1.00
## base        -1.35e+01   2.12e+04    0.00     1.00
## begin        2.23e+01   2.97e+04    0.00     1.00
## believ       3.23e+01   2.14e+04    0.00     1.00
## best        -8.20e+00   1.33e+03   -0.01     1.00
## better       4.26e+01   2.36e+04    0.00     1.00
## book         4.30e+00   2.02e+04    0.00     1.00
## bring        1.61e+01   6.77e+04    0.00     1.00
## busi        -4.80e+00   1.00e+04    0.00     1.00
## buy          4.17e+01   3.89e+04    0.00     1.00
## call        -1.15e+00   1.11e+04    0.00     1.00
## can          3.76e+00   7.67e+03    0.00     1.00
## case        -3.37e+01   2.88e+04    0.00     1.00
## chang       -2.72e+01   2.22e+04    0.00     1.00
## check        1.43e+00   1.96e+04    0.00     1.00
## click        1.38e+01   7.08e+03    0.00     1.00
## com          1.94e+00   4.04e+03    0.00     1.00
## come        -1.17e+00   1.51e+04    0.00     1.00
## comment     -3.25e+00   3.39e+04    0.00     1.00
## communic     1.58e+01   8.96e+03    0.00     1.00
## compani      4.78e+00   9.19e+03    0.00     1.00
## complet     -1.36e+01   2.02e+04    0.00     1.00
## confer      -7.50e-01   8.56e+03    0.00     1.00
## confirm     -1.30e+01   1.51e+04    0.00     1.00
## contact      1.53e+00   1.26e+04    0.00     1.00
## continu      1.49e+01   1.54e+04    0.00     1.00
## contract    -1.30e+01   1.50e+04    0.00     1.00
## copi        -4.27e+01   3.07e+04    0.00     1.00
## corp         1.61e+01   2.71e+04    0.00     1.00
## corpor      -8.29e-01   2.82e+04    0.00     1.00
## cost        -1.94e+00   1.83e+04    0.00     1.00
## cours        1.67e+01   1.83e+04    0.00     1.00
## creat        1.34e+01   3.95e+04    0.00     1.00
## credit       2.62e+01   1.31e+04    0.00     1.00
## crenshaw     9.99e+01   6.77e+04    0.00     1.00
## current      3.63e+00   1.71e+04    0.00     1.00
## custom       1.83e+01   1.01e+04    0.00     1.00
## data        -2.61e+01   2.27e+04    0.00     1.00
## date        -2.79e+00   1.70e+04    0.00     1.00
## day         -6.10e+00   5.87e+03    0.00     1.00
## deal        -1.13e+01   1.45e+04    0.00     1.00
## dear        -2.31e+00   2.31e+04    0.00     1.00
## depart      -4.07e+01   2.51e+04    0.00     1.00
## deriv       -4.97e+01   3.59e+04    0.00     1.00
## design      -7.92e+00   2.94e+04    0.00     1.00
## detail       1.20e+01   2.30e+04    0.00     1.00
## develop      5.98e+00   9.45e+03    0.00     1.00
## differ      -2.29e+00   1.07e+04    0.00     1.00
## direct      -2.05e+01   3.19e+04    0.00     1.00
## director    -1.77e+01   1.79e+04    0.00     1.00
## discuss     -1.05e+01   1.92e+04    0.00     1.00
## doc         -2.60e+01   2.60e+04    0.00     1.00
## don          2.13e+01   1.46e+04    0.00     1.00
## done         6.83e+00   1.88e+04    0.00     1.00
## due         -4.16e+00   3.53e+04    0.00     1.00
## ect          8.69e-01   5.34e+03    0.00     1.00
## edu         -2.12e-01   6.92e+02    0.00     1.00
## effect       1.95e+01   2.10e+04    0.00     1.00
## effort       1.61e+01   5.67e+04    0.00     1.00
## either      -2.74e+01   4.00e+04    0.00     1.00
## email        3.83e+00   1.19e+04    0.00     1.00
## end         -1.31e+01   2.94e+04    0.00     1.00
## energi      -1.62e+01   1.65e+04    0.00     1.00
## engin        2.66e+01   2.39e+04    0.00     1.00
## enron       -8.79e+00   5.72e+03    0.00     1.00
## etc          9.47e-01   1.57e+04    0.00     1.00
## even        -1.65e+01   2.29e+04    0.00     1.00
## event        1.69e+01   1.85e+04    0.00     1.00
## expect      -1.18e+01   1.91e+04    0.00     1.00
## experi       2.46e+00   2.24e+04    0.00     1.00
## fax          3.54e+00   3.39e+04    0.00     1.00
## feel         2.60e+00   2.35e+04    0.00     1.00
## file        -2.94e+01   2.16e+04    0.00     1.00
## final        8.07e+00   5.01e+04    0.00     1.00
## financ      -9.12e+00   7.52e+03    0.00     1.00
## financi     -9.75e+00   1.73e+04    0.00     1.00
## find        -2.62e+00   9.73e+03    0.00     1.00
## first       -4.67e-01   2.04e+04    0.00     1.00
## follow       1.77e+01   3.08e+03    0.01     1.00
## form         8.48e+00   1.67e+04    0.00     1.00
## forward     -3.48e+00   1.86e+04    0.00     1.00
## free         6.11e+00   8.12e+03    0.00     1.00
## friday      -1.15e+01   2.00e+04    0.00     1.00
## full         2.13e+01   2.19e+04    0.00     1.00
## futur        4.15e+01   1.44e+04    0.00     1.00
## gas         -3.90e+00   4.16e+03    0.00     1.00
## get          5.15e+00   9.74e+03    0.00     1.00
## gibner       2.90e+01   2.46e+04    0.00     1.00
## give        -2.52e+01   2.13e+04    0.00     1.00
## given       -2.19e+01   5.43e+04    0.00     1.00
## good         5.40e+00   1.62e+04    0.00     1.00
## great        1.22e+01   1.09e+04    0.00     1.00
## group        5.26e-01   1.04e+04    0.00     1.00
## happi        1.94e-02   1.20e+04    0.00     1.00
## hear         2.89e+01   2.28e+04    0.00     1.00
## hello        2.17e+01   1.36e+04    0.00     1.00
## help         1.73e+01   2.79e+03    0.01     1.00
## high        -1.98e+00   2.55e+04    0.00     1.00
## home         5.97e+00   8.96e+03    0.00     1.00
## hope        -1.44e+01   2.18e+04    0.00     1.00
## hou          6.85e+00   6.44e+03    0.00     1.00
## hour         2.48e+00   1.33e+04    0.00     1.00
## houston     -1.85e+01   7.31e+03    0.00     1.00
## howev       -3.45e+01   3.56e+04    0.00     1.00
## http         2.53e+01   2.11e+04    0.00     1.00
## idea        -1.84e+01   3.89e+04    0.00     1.00
## immedi       6.29e+01   3.35e+04    0.00     1.00
## import      -1.86e+00   2.24e+04    0.00     1.00
## includ      -3.45e+00   1.80e+04    0.00     1.00
## increas      6.48e+00   2.33e+04    0.00     1.00
## industri    -3.16e+01   2.37e+04    0.00     1.00
## info        -1.25e+00   4.86e+03    0.00     1.00
## inform       2.08e+01   8.55e+03    0.00     1.00
## interest     2.70e+01   1.16e+04    0.00     1.00
## intern      -7.99e+00   3.35e+04    0.00     1.00
## internet     8.75e+00   1.10e+04    0.00     1.00
## interview   -1.64e+01   1.87e+04    0.00     1.00
## invest       3.20e+01   2.39e+04    0.00     1.00
## invit        4.30e+00   2.22e+04    0.00     1.00
## involv       3.81e+01   3.32e+04    0.00     1.00
## issu        -3.71e+01   3.40e+04    0.00     1.00
## john        -5.33e-01   2.86e+04    0.00     1.00
## join        -3.82e+01   2.33e+04    0.00     1.00
## juli        -1.36e+01   3.01e+04    0.00     1.00
## just        -1.02e+01   1.11e+04    0.00     1.00
## kaminski    -1.81e+01   6.03e+03    0.00     1.00
## keep         1.87e+01   2.78e+04    0.00     1.00
## kevin       -3.78e+01   4.74e+04    0.00     1.00
## know         1.28e+01   1.53e+04    0.00     1.00
## last         1.05e+00   1.37e+04    0.00     1.00
## let         -2.76e+01   1.46e+04    0.00     1.00
## life         5.81e+01   3.86e+04    0.00     1.00
## like         5.65e+00   7.66e+03    0.00     1.00
## line         8.74e+00   1.24e+04    0.00     1.00
## link        -6.93e+00   1.34e+04    0.00     1.00
## list        -8.69e+00   2.15e+03    0.00     1.00
## locat        2.07e+01   1.60e+04    0.00     1.00
## london       6.75e+00   1.64e+04    0.00     1.00
## long        -1.49e+01   1.93e+04    0.00     1.00
## look        -7.03e+00   1.56e+04    0.00     1.00
## lot         -1.96e+01   1.32e+04    0.00     1.00
## made         2.82e+00   2.74e+04    0.00     1.00
## mail         7.58e+00   1.02e+04    0.00     1.00
## make         2.90e+01   1.53e+04    0.00     1.00
## manag        6.01e+00   1.45e+04    0.00     1.00
## mani         1.89e+01   1.44e+04    0.00     1.00
## mark        -3.35e+01   3.21e+04    0.00     1.00
## market       7.90e+00   8.01e+03    0.00     1.00
## may         -9.43e+00   1.40e+04    0.00     1.00
## mean         6.08e-01   2.95e+04    0.00     1.00
## meet        -1.06e+00   1.26e+04    0.00     1.00
## member       1.38e+01   2.34e+04    0.00     1.00
## mention     -2.28e+01   2.71e+04    0.00     1.00
## messag       1.72e+01   2.56e+03    0.01     0.99
## might        1.24e+01   1.75e+04    0.00     1.00
## model       -2.29e+01   1.05e+04    0.00     1.00
## monday      -1.03e+00   3.23e+04    0.00     1.00
## money        3.26e+01   1.32e+04    0.00     1.00
## month       -3.73e+00   1.11e+04    0.00     1.00
## morn        -2.64e+01   3.40e+04    0.00     1.00
## move        -3.83e+01   3.01e+04    0.00     1.00
## much         3.78e-01   1.39e+04    0.00     1.00
## name         1.67e+01   1.32e+04    0.00     1.00
## need         8.44e-01   1.22e+04    0.00     1.00
## net          1.26e+01   2.20e+04    0.00     1.00
## new          1.00e+00   1.01e+04    0.00     1.00
## next.        1.49e+01   1.72e+04    0.00     1.00
## note         1.45e+01   2.29e+04    0.00     1.00
## now          3.79e+01   1.22e+04    0.00     1.00
## number      -9.62e+00   1.59e+04    0.00     1.00
## offer        1.17e+01   1.08e+04    0.00     1.00
## offic       -1.34e+01   2.31e+04    0.00     1.00
## one          1.24e+01   6.65e+03    0.00     1.00
## onlin        3.59e+01   1.66e+04    0.00     1.00
## open         2.11e+01   2.96e+04    0.00     1.00
## oper        -1.70e+01   2.76e+04    0.00     1.00
## opportun    -4.13e+00   1.92e+04    0.00     1.00
## option      -1.09e+00   9.33e+03    0.00     1.00
## order        6.53e+00   1.24e+04    0.00     1.00
## origin       3.23e+01   3.82e+04    0.00     1.00
## part         4.59e+00   3.48e+04    0.00     1.00
## particip    -1.15e+01   1.74e+04    0.00     1.00
## peopl       -1.86e+01   1.44e+04    0.00     1.00
## per          1.37e+01   1.27e+04    0.00     1.00
## person       1.87e+01   9.58e+03    0.00     1.00
## phone       -6.96e+00   1.17e+04    0.00     1.00
## place        9.01e+00   3.66e+04    0.00     1.00
## plan        -1.83e+01   6.32e+03    0.00     1.00
## pleas       -7.96e+00   9.48e+03    0.00     1.00
## point        5.50e+00   3.40e+04    0.00     1.00
## posit       -1.54e+01   2.32e+04    0.00     1.00
## possibl     -1.37e+01   2.49e+04    0.00     1.00
## power       -5.64e+00   1.17e+04    0.00     1.00
## present     -6.16e+00   1.28e+04    0.00     1.00
## price        3.43e+00   7.85e+03    0.00     1.00
## problem      1.26e+01   9.76e+03    0.00     1.00
## process     -2.96e-01   1.19e+04    0.00     1.00
## product      1.02e+01   1.34e+04    0.00     1.00
## program      1.44e+00   1.18e+04    0.00     1.00
## project      2.17e+00   1.50e+04    0.00     1.00
## provid       2.42e-01   1.86e+04    0.00     1.00
## public      -5.25e+01   2.34e+04    0.00     1.00
## put         -1.05e+01   2.68e+04    0.00     1.00
## question    -3.47e+01   1.86e+04    0.00     1.00
## rate        -3.11e+00   1.32e+04    0.00     1.00
## read        -1.53e+01   2.14e+04    0.00     1.00
## real         2.05e+01   2.36e+04    0.00     1.00
## realli      -2.67e+01   4.64e+04    0.00     1.00
## receiv       5.77e-01   1.58e+04    0.00     1.00
## recent      -2.07e+00   1.78e+04    0.00     1.00
## regard      -3.67e+00   1.51e+04    0.00     1.00
## relat       -5.11e+01   1.79e+04    0.00     1.00
## remov        2.33e+01   2.48e+04    0.00     1.00
## repli        1.54e+01   2.92e+04    0.00     1.00
## report      -1.48e+01   1.48e+04    0.00     1.00
## request     -1.23e+01   1.17e+04    0.00     1.00
## requir       5.00e-01   2.94e+04    0.00     1.00
## research    -2.83e+01   1.55e+04    0.00     1.00
## resourc     -2.73e+01   3.52e+04    0.00     1.00
## respond      2.97e+01   3.89e+04    0.00     1.00
## respons     -1.96e+01   3.67e+04    0.00     1.00
## result      -5.00e-01   3.14e+04    0.00     1.00
## resum       -9.22e+00   2.10e+04    0.00     1.00
## return       1.75e+01   1.84e+04    0.00     1.00
## review      -4.82e+00   1.01e+04    0.00     1.00
## right        2.31e+01   1.59e+04    0.00     1.00
## risk        -4.00e+00   1.72e+04    0.00     1.00
## robert      -2.10e+01   2.91e+04    0.00     1.00
## run         -5.16e+01   4.43e+04    0.00     1.00
## say          7.37e+00   2.22e+04    0.00     1.00
## schedul      1.92e+00   3.58e+04    0.00     1.00
## school      -3.87e+00   2.88e+04    0.00     1.00
## secur       -1.60e+01   2.20e+03   -0.01     0.99
## see         -1.12e+01   1.29e+04    0.00     1.00
## send        -2.43e+01   1.22e+04    0.00     1.00
## sent        -1.49e+01   2.20e+04    0.00     1.00
## servic      -7.16e+00   1.24e+04    0.00     1.00
## set         -9.35e+00   2.63e+04    0.00     1.00
## sever        2.04e+01   3.09e+04    0.00     1.00
## shall        1.93e+01   3.07e+04    0.00     1.00
## shirley     -7.13e+01   6.33e+04    0.00     1.00
## short       -8.97e+00   1.72e+04    0.00     1.00
## sinc        -3.44e+00   3.55e+04    0.00     1.00
## sincer      -2.07e+01   3.51e+04    0.00     1.00
## site         8.69e+00   1.50e+04    0.00     1.00
## softwar      2.57e+01   1.06e+04    0.00     1.00
## soon         2.35e+01   3.73e+04    0.00     1.00
## sorri        6.04e+00   2.30e+04    0.00     1.00
## special      1.78e+01   2.76e+04    0.00     1.00
## specif      -2.34e+01   3.08e+04    0.00     1.00
## start        1.44e+01   1.90e+04    0.00     1.00
## state        1.22e+01   1.68e+04    0.00     1.00
## still        3.88e+00   2.62e+04    0.00     1.00
## stinson     -4.35e+01   2.70e+04    0.00     1.00
## student     -1.81e+01   2.19e+04    0.00     1.00
## subject      3.04e+01   1.05e+04    0.00     1.00
## success      4.34e+00   2.78e+04    0.00     1.00
## suggest     -3.84e+01   4.47e+04    0.00     1.00
## support     -1.54e+01   1.98e+04    0.00     1.00
## sure        -5.50e+00   2.08e+04    0.00     1.00
## system       3.78e+00   9.15e+03    0.00     1.00
## take         5.73e+00   1.72e+04    0.00     1.00
## talk        -1.01e+01   2.02e+04    0.00     1.00
## team         7.94e+00   2.57e+04    0.00     1.00
## term         2.01e+01   2.30e+04    0.00     1.00
## thank       -3.89e+01   1.06e+04    0.00     1.00
## thing        2.58e+01   1.34e+04    0.00     1.00
## think       -1.22e+01   2.08e+04    0.00     1.00
## thought      1.24e+01   3.02e+04    0.00     1.00
## thursday    -1.49e+01   3.26e+04    0.00     1.00
## time        -5.92e+00   8.33e+03    0.00     1.00
## today       -1.76e+01   1.96e+04    0.00     1.00
## togeth      -2.35e+01   1.87e+04    0.00     1.00
## trade       -1.76e+01   1.48e+04    0.00     1.00
## tri          9.28e-01   1.28e+04    0.00     1.00
## tuesday     -2.81e+01   3.96e+04    0.00     1.00
## two         -2.57e+01   1.84e+04    0.00     1.00
## type        -1.45e+01   2.75e+04    0.00     1.00
## understand   9.31e+00   2.34e+04    0.00     1.00
## unit        -4.02e+00   3.01e+04    0.00     1.00
## univers      1.23e+01   2.20e+04    0.00     1.00
## updat       -1.51e+01   1.45e+04    0.00     1.00
## use         -1.39e+01   9.38e+03    0.00     1.00
## valu         9.02e-01   1.36e+04    0.00     1.00
## version     -3.61e+01   2.94e+04    0.00     1.00
## vinc        -3.73e+01   8.65e+03    0.00     1.00
## visit        2.58e+01   1.17e+04    0.00     1.00
## vkamin      -6.65e+01   5.70e+04    0.00     1.00
## want        -2.56e+00   1.11e+04    0.00     1.00
## way          1.34e+01   1.14e+04    0.00     1.00
## web          2.79e+00   1.69e+04    0.00     1.00
## websit      -2.56e+01   1.85e+04    0.00     1.00
## wednesday   -1.53e+01   2.64e+04    0.00     1.00
## week        -6.79e+00   1.05e+04    0.00     1.00
## well        -2.22e+01   9.71e+03    0.00     1.00
## will        -1.12e+01   5.98e+03    0.00     1.00
## wish         1.17e+01   3.17e+04    0.00     1.00
## within       2.90e+01   2.16e+04    0.00     1.00
## without      1.94e+01   1.76e+04    0.00     1.00
## work        -1.10e+01   1.16e+04    0.00     1.00
## write        4.41e+01   2.82e+04    0.00     1.00
## www         -7.87e+00   2.22e+04    0.00     1.00
## year        -1.01e+01   1.04e+04    0.00     1.00
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4409.49  on 4009  degrees of freedom
## Residual deviance:   13.46  on 3679  degrees of freedom
## AIC: 675.5
## 
## Number of Fisher Scoring iterations: 25
```

```r
predLog <- predict(spamLog, type = "response")
sum(predLog < 1e-05)
```

```
## [1] 3046
```

```r
sum(predLog > 0.99999)
```

```
## [1] 954
```

```r
sum(predLog > 1e-05 & predLog < 0.99999)
```

```
## [1] 10
```

```r
# Accuracy
tLog <- table(train$spam, predLog >= 0.5)
(tLog[1, 1] + tLog[2, 2])/sum(tLog)
```

```
## [1] 0.999
```

```r
# AUC
library(ROCR)
```

```
## Loading required package: gplots
## KernSmooth 2.23 loaded
## Copyright M. P. Wand 1997-2009
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```r

predROCR = prediction(predLog, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) 

```r

# Compute AUC
performance(predROCR, "auc")@y.values
```

```
## [[1]]
## [1] 1
```

```r


# CART Model
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data = train, method = "class")
prp(spamCART)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) 

```r

# Predict using the trainig set.
predTrain <- predict(spamCART)[, 2]
# Accuracy on the training set
tCART <- table(train$spam, predTrain >= 0.5)
(tCART[1, 1] + tCART[2, 2])/(sum(tCART))
```

```
## [1] 0.9424
```

```r

# AUC of the CART model
predROCRCART = prediction(predTrain, train$spam)
perfROCRCART = performance(predROCRCART, "tpr", "fpr")
performance(predROCRCART, "auc")@y.values
```

```
## [[1]]
## [1] 0.9696
```

```r


# Random Forest model
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
set.seed(123)
spamRF <- randomForest(spam ~ ., data = train, method = "class")

# Accuracy of RF Model
predRF <- predict(spamRF, type = "prob")[, 2]
tRF <- table(train$spam, predRF >= 0.5)
(tRF[1, 1] + tRF[2, 2])/(sum(tRF))
```

```
## [1] 0.9803
```

```r

# Performance of RF Model
predROCRRF = prediction(predRF, train$spam)
performance(predROCRRF, "auc")@y.values
```

```
## [[1]]
## [1] 0.998
```


## *EVALUATING THE MODEL ON THE TESTING SET*

```r
# Testing set accuracy
predTestLog <- predict(spamLog, newdata = test, type = "response")
t2 <- table(test$spam, predTestLog >= 0.5)
(t2[1, 1] + t2[2, 2])/(sum(t2))
```

```
## [1] 0.9505
```

```r

# ROC Curve
library(ROCR)

predROCRLog = prediction(predTestLog, test$spam)

# Compute AUC
performance(predROCRLog, "auc")@y.values
```

```
## [[1]]
## [1] 0.9628
```

```r

# CART Test Accuracy and performance
predTestCART <- predict(spamCART, newdata = test)[, 2]
t3 <- table(test$spam, predTestCART >= 0.5)
(t3[1, 1] + t3[2, 2])/(sum(t3))
```

```
## [1] 0.9395
```

```r
predROCRCART = prediction(predTestCART, test$spam)

# Compute AUC
performance(predROCRCART, "auc")@y.values
```

```
## [[1]]
## [1] 0.9632
```


## *ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS*
Thus far, we have used a threshold of 0.5 as the cutoff for predicting that an email message is spam, and we have used accuracy as one of our measures of model quality. As we have previously learned, these are good choices when we have no preference for different types of errors (false positives vs. false negatives), but other choices might be better if we assign a higher cost to one type of error.

Consider the case of an email provider using the spam filter we have developed. The email provider moves all of the emails flagged as spam to a separate "Junk Email" folder, meaning those emails are not displayed in the main inbox. The emails not flagged as spam by the algorithm are displayed in the inbox. Many of this provider's email users never check the spam folder, so they will never see emails delivered there.

A false negative means the model labels a spam email as ham. This results in a spam email being displayed in the main inbox.

A false positive means the model labels a ham email as spam. This results in a ham email being sent to the Junk Email folder.

A false negative is largely a nuisance (the user will need to delete the unsolicited email). However a false positive can be very costly, since the user might completely miss an important email due to it being delivered to the spam folder. Therefore, the false positive is more costly.

A false negative results in spam reaching a user's main inbox, which is a nuisance. A user who is particularly annoyed by such spam would assign a particularly high cost to a false negative.

A false positive results in ham being sent to a user's Junk Email folder. While the user might catch the mistake upon checking the Junk Email folder, users who never check this folder will miss the email, incurring a particularly high cost.

While before many users would completely miss a ham email labeled as spam (false positive), now users will not miss an email after this sort of mistake. As a result, the cost of a false positive has been decreased.

While using expert opinion is practical, it is not personalized (we would use the same cost for all users). Likewise, a random sample of user preferences doesn't enable personalized costs for each user.

While a survey of all users would enable personalization, it is impractical to obtain survey results from all or most of the users.

While it's impractical to survey all users, it is easy to automatically collect their usage patterns. This could enable us to select higher regression thresholds for users who rarely check their Junk Email folder but lower thresholds for users who regularly check the folder.

## *INTEGRATING WORD COUNT INFORMATION*
While we have thus far mostly dealt with frequencies of specific words in our analysis, we can extract other information from text. The last two sections of this problem will deal with two other types of information we can extract.

First, we will use the number of words in the each email as an independent variable. We can use the original document term matrix called dtm for this task. The document term matrix has documents (in this case, emails) as its rows, terms (in this case word stems) as its columns, and frequencies as its values. As a result, the sum of all the elements in a row of the document term matrix is equal to the number of terms present in this document

```r
# Obtain the word counts for each email with the command wordCount =
# rowSums(as.matrix(dtm)) # Gives an error because of size. Thus run the
# following.
library(slam)
wordCount = rollup(dtm, 2, FUN = sum)$v
# Histogram of wordCount
hist(wordCount)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r
# Histogram of log(wordCount)
hist(log(wordCount))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 

```r
# Add the variable logWordCount to emailsSparse
emailsSparse$logWordCount <- log(wordCount)
# boxplot
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-53.png) 

```r

# train
train2 <- subset(emailsSparse, spl == T)
test2 <- subset(emailsSparse, spl == F)

# CART Model
spam2CART <- rpart(spam ~ ., data = train2, method = "class")

# test-set Accuracy of spam2CART
pred2CART <- predict(spam2CART, newdata = test2)[, 2]
t2CART <- table(test2$spam, pred2CART >= 0.5)
(t2CART[1, 1] + t2CART[2, 2])/sum(t2CART)
```

```
## [1] 0.9302
```

```r

# Performance
predROCR2CART = prediction(pred2CART, test$spam)
performance(predROCR2CART, "auc")@y.values
```

```
## [[1]]
## [1] 0.9582
```

```r

# Random Forest
set.seed(123)
spam2RF <- randomForest(spam ~ ., data = train2, method = "class")

# Accuracy of RF Model
pred2RF <- predict(spam2RF, type = "prob")[, 2]
t2RF <- table(train2$spam, pred2RF >= 0.5)
(t2RF[1, 1] + t2RF[2, 2])/(sum(t2RF))
```

```
## [1] 0.9805
```

```r

# Performance of RF Model
predROCR2RF = prediction(pred2RF, train2$spam)
performance(predROCR2RF, "auc")@y.values
```

```
## [[1]]
## [1] 0.9973
```


# *USING 2-GRAMS TO PREDICT SPAM*
Another source of information that might be extracted from text is the frequency of various n-grams. An n-gram is a sequence of n consecutive words in the document. For instance, for the document "Text analytics rocks!", which we would preprocess to "text analyt rock", the 1-grams are "text", "analyt", and "rock", the 2-grams are "text analyt" and "analyt rock", and the only 3-gram is "text analyt rock". n-grams are order-specific, meaning the 2-grams "text analyt" and "analyt text" are considered two separate n-grams. We can see that so far our analysis has been extracting only 1-grams.

In this last subproblem, we will add 2-grams to our predictive model. Begin by installing and loading the RTextTools package.

```r
library(RTextTools)
```

```
## Loading required package: SparseM
## 
## Attaching package: 'SparseM'
## 
## The following object is masked from 'package:base':
## 
##     backsolve
```

```r
# Create a document term matrix containing all 2-grams in our dataset
dtm2gram = create_matrix(as.character(corpus), ngramLength = 2)
dtm2gram
```

```
## A document-term matrix (5728 documents, 304449 terms)
## 
## Non-/sparse entries: 707213/1743176659
## Sparsity           : 100%
## Maximal term length: 35 
## Weighting          : term frequency (tf)
```

```r
# Filter out sparse terms by keeping only terms that appear in at least 5%
# or more of the documents
spdtm2Gram <- removeSparseTerms(dtm2gram, 0.95)
spdtm2Gram
```

```
## A document-term matrix (5728 documents, 35 terms)
## 
## Non-/sparse entries: 25293/175187
## Sparsity           : 87%
## Maximal term length: 16 
## Weighting          : term frequency (tf)
```

```r

# Create a dataframe from spdtm2Gram
emailsSparse2gram <- as.data.frame(as.matrix(spdtm2Gram))
# Convert the column names of emailsSparse2gram to valid names using
# make.names()
colnames(emailsSparse2gram) <- make.names(colnames(emailsSparse2gram), unique = T)
# Combine the original emailsSparse with emailsSparse2gram into a final data
# frame
emailsCombined = cbind(emailsSparse, emailsSparse2gram)
# Split the above data frame into training and testing sets
trainCombined <- subset(emailsCombined, spl == T)
testCombined <- subset(emailsCombined, spl == F)
# Use trainCombined to train a CART tree with the default parameter
spamCARTCombined <- rpart(spam ~ ., data = trainCombined, method = "class")
# Tree
prp(spamCARTCombined, varlen = 0)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
# Perform test-set predictions using the new CART. Accuracy
predCARTCombined <- predict(spamCARTCombined, newdata = testCombined)[, 2]
tCARTCombined <- table(testCombined$spam, predCARTCombined >= 0.5)
(tCARTCombined[1, 1] + tCARTCombined[2, 2])/sum(tCARTCombined)
```

```
## [1] 0.9354
```

```r

# Performance of the CART Model
predROCRCombCART = prediction(predCARTCombined, testCombined$spam)
performance(predROCRCombCART, "auc")@y.values
```

```
## [[1]]
## [1] 0.9648
```

```r

# Use trainCombined to train a random forest with the default parameters
set.seed(123)
spamRFCombined <- randomForest(spam ~ ., data = trainCombined, method = "class")
# Perform test-set predictions using the new RF model. Accuracy
predRFCombined <- predict(spamRFCombined, newdata = testCombined, type = "prob")[, 
    2]
tRFCombined <- table(testCombined$spam, predRFCombined >= 0.5)
(tRFCombined[1, 1] + tRFCombined[2, 2])/sum(tRFCombined)
```

```
## [1] 0.9796
```

```r
# What is the test-set AUC of spamRFcombined? Performance of RF model
predROCRCombRF = prediction(predRFCombined, testCombined$spam)
performance(predROCRCombRF, "auc")@y.values
```

```
## [[1]]
## [1] 0.9977
```


For this problem, adding 2-grams did not dramatically improve our test-set performance. Adding n-grams is most effective in large datasets. Given the billions of emails sent each day, it's reasonable to expect that email providers would be able to construct datasets large enough for n-grams to provide useful predictive power.
