MITx: 15.071x The Analytics Edge - Letter Recognition
========================================================
### Tarek Dib
### April 7, 2014

# *Introduction*
One of the earliest applications of the predictive analytics methods we have studied so far in this class was to automatically recognize letters. One application is for post office machines to sort mail. In this problem, we will build a model that uses statistics of images of four letters in the Roman alphabet -- A, B, P, and R -- to predict which letter a particular image corresponds to.

This is slightly different from the problems we have considered so far. We have previously focused on binary classification problems (e.g., predicting whether an individual voted or not, earns more than $50,000 annually or not, is at risk for a certain disease or not, etc.). In this problem, we have more than two classifications that are possible for each observation. Such problems are called multiclass classification problems.

The file letters_ABPR.csv contains 3116 observations, each of which corresponds to a certain image of one of the four letters A, B, P and R. The images came from 20 different fonts, which were then randomly distorted to produce the final images; each such distorted image is represented as a collection of pixels, each of which is "on" or "off". For each such distorted image, we have available certain statistics of the image in terms of these pixels, as well as which of the four letters the image is. This data comes from the UCI Machine Learning Repository.

# *Variables*
The available variables include:

    letter = the letter that the image corresponds to (A, B, P or R)
    xbox = the horizontal position of where the smallest box covering the letter shape begins.
    ybox = the vertical position of where the smallest box covering the letter shape begins.
    width = the width of this smallest box.
    height = the height of this smallest box.
    onpix = the total number of "on" pixels in the character image
    xbar = the mean horizontal position of all of the "on" pixels
    ybar = the mean vertical position of all of the "on" pixels
    x2bar = the mean squared horizontal position of all of the "on" pixels in the image
    y2bar = the mean squared vertical position of all of the "on" pixels in the image
    xybar = the mean of the product of the horizontal and vertical position of all of the "on" pixels in the image
    x2ybar = the mean of the product of the squared horizontal position and the vertical position of all of the "on" pixels
    xy2bar = the mean of the product of the horizontal position and the squared vertical position of all of the "on" pixels
    xedge = the mean number of edges (the number of times an "off" pixel is followed by an "on" pixel, or the image boundary is hit) as the image is scanned from left to right, along the whole vertical length of the image
    xedgeycor = the mean of the product of the number of horizontal edges at each vertical position and the vertical position 
    yedge = the mean number of edges as the image is scanned from top to bottom, along the whole horizontal length of the image
    yedgexcor = the mean of the product of the number of vertical edges at each horizontal position and the horizontal position
    
# *Predicting B or not B*

```r
# Read Data
letters = read.csv("letters_ABPR.csv")
# create a new variable isB in the dataframe, which takes the value 'yes'
# if the observation corresponds to the letter B, and 'no' if it does not
letters$isB = as.factor(letters$letter == "B")

# Set seed
set.seed(1000)
library(caTools)

split = sample.split(letters$isB, SplitRatio = 0.5)

# Split up the data using subset
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

# Baseline model
t_bl <- table(test$isB)
# Accuracy
t_bl[1]/sum(t_bl)
```

```
##  FALSE 
## 0.7542
```

```r

# Build a CART model using the training set
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data = train, method = "class")
# Predict
pred <- predict(CARTb, newdata = test, type = "class")
# Accuray
t_CART <- table(test$isB, pred)
(t_CART[1, 1] + t_CART[2, 2])/sum(t_CART)
```

```
## [1] 0.9358
```

