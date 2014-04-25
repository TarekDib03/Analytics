# Get the current directory
  getwd()
# Change the current directory
  setwd("Data")
# Read the csv file
  USDA = read.csv("USDA.csv")
# Structure of the dataset
  str(USDA)
# Statistical summary
  summary(USDA)
# Attach column names
  attach(USDA)

# Basic Data Analysis

# Finding the index of the food with highest sodium levels
  which.max(Sodium)
# Get names of variables in the dataset
  names(USDA)
# Get the name of the food with highest sodium levels
  Description[which.max(Sodium)]
# Create a subset of the foods with sodium content above 10,000mg
  HighSodium = subset(USDA, Sodium>10000)
# Output names of the foods with high sodium content
  HighSodium$Description
# Find amount of sodium in caviar
  USDA$Sodium[match("CAVIAR", USDA$Description)]
# Summary function over Sodium vector
  summary(USDA$Sodium)
# Standard deviation
  sd(USDA$Sodium, na.rm = TRUE)
  
  
# Plots

# Scatter plot
  plot(Protein, TotalFat, xlab="Protein", ylab = "Fat", main = "Fat vs. Protein", col = "red")
# Creating a histogram
  hist(VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C")
  hist(VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,300), breaks=200)
# Boxplots
  boxplot(Sugar, ylab = "Sugar (g)", main = "Boxplot of Sugar")


# Adding a variable

# Creating a variable that takes value 1 if the food has higher sodium than average, 0 otherwise
  HighSodium = as.numeric(Sodium > mean(Sodium, na.rm=TRUE))
  str(HighSodium)
# Adding the variable to the dataset
  USDA$HighSodium = as.numeric(Sodium > mean(Sodium, na.rm=TRUE))
# Similarly for HighProtein, HigCarbs, HighFat
  USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
  USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
  USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))


# Video 6 - Summary Tables

# How many foods have higher sodium level than average?
  table(USDA$HighSodium)
# How many foods have both high sodium and high fat?
  table(USDA$HighSodium, USDA$HighFat)
# Average amount of iron sorted by high and low protein?
  tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)
# Maximum level of Vitamin C in hfoods with high and low carbs?
  tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
# Using summary function with tapply
  tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)
