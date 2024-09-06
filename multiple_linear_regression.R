# Multiple Linear Regression

# Load and install necessary Libraries
#install.packages('caTools')
library(tidyverse)
library(caTools) # for data spliting
#install.packages("car")
library(car) # to test multicollinearity
#install.packages("lmtest")
library(lmtest) # for Autocorrelation (Durbin-Watson test)


##########################################################
rm(list=ls())

#set working directory
setwd("C:/Users/Admin/Documents/Machine Learning A-Z (Codes and Datasets)/
      Part 2 - Regression/Section 5 -  Multiple Linear Regression/R")


# Importing the dataset
dataset = read.csv('50_Startups.csv')

## having a glimpse of the data
str(data)      # Check the structure of the dataset
summary(dataset)
glimpse(dataset)

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8) 
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling is not needed in multiple linear regression 
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)
summary(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
plot(y_pred, col = "blue", pch = 19, ylab="values",
     main="Scatter Plot for predictiona and actual value")
points(test_set$Profit, col = "red", pch = 19)
# Adding a legend to differentiate the two variables
legend("topright", legend=c("predict", "actual"), col=c("blue", "red"), pch=19)


### Checking the assumptions on the built model
# Step 2: Diagnostic Plots
par(mfrow = c(2, 2))
plot(regressor)

# Step 3: Check for Multicollinearity
vif(regressor)

# Step 4: Check for Autocorrelation (Durbin-Watson test)
dwtest(regressor)

# Perform outlier test
outlierTest(model)

# Extract residuals
residuals <- resid(model)

# Perform Shapiro-Wilk normality test
shapiro.test(residuals)

# Breusch-Pagan test for heteroscedasticity
bptest(model)
