# VIDEO 2

# Basic Calculations
8*6
2^16
2^
8*6
8*10

# Functions
sqrt(2)
abs(-65)
?sqrt

# Variables
SquareRoot2 = sqrt(2)
SquareRoot2
HoursYear <- 365*24
HoursYear
ls()


# VIDEO 3 

# Vectors
c(2,3,5,8,13)
Country = c("Brazil", "China", "India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)
Country
LifeExpectancy
c("Brazil",74,"China",76)
Country[1]
LifeExpectancy[3]
Sequence = seq(1,100,2)
Sequence

# Data Frames
Data = data.frame(Country, LifeExpectancy)
Data
Population = c(199000,1390000,1240000,7997,318000)
Data2 = cbind(Data,Population)
Data2
Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewData = data.frame(Country, LifeExpectancy, Population)
NewData
Data3 = rbind(Data2, NewData)
Data3


# VIDEO 4

# Loading csv files
WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO)

# Subsetting
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)

# Writing csv files
write.csv(WHO_Europe, "WHO_Europe.csv")

# Removing variables
rm(WHO_Europe)


# VIDEO 5

# Basic data analysis 

mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)

which.min(WHO$Under15)
WHO$Country[86]

which.max(WHO$Under15)
WHO$Country[124]

sort(WHO$Under15)


# Scatterplot
plot(WHO$GNI, WHO$FertilityRate)

# Subsetting
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5) 
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]

# Histograms
hist(WHO$CellularSubscribers)

# Boxplot
boxplot(WHO$LifeExpectancy ~ WHO$Region)

boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "Region", ylab = "Life Expectancy", main = "Life Expectancy of Countries by Region")

# Summary Tables
table(WHO$Region)

tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$LiteracyRate, WHO$Region, min)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)
 