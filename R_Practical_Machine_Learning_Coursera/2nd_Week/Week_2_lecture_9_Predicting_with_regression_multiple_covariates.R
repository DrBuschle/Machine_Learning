# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle

rm(list = ls())

# Week 2 - Lecture 9 - Prediction with Regression Multiple Covariates  ####



# Example: Wage data ####

library(ISLR) # required for the data set
library(ggplot2)  # required for exploratory analysis
library(caret)  # required for prediction

data(Wage)
Wage <- subset(Wage, select = - c(logwage))
# subset all the data except the one you are trying to predict

summary(Wage)

# year           age                     maritl           race                   education                     region    
# Min.   :2003   Min.   :18.00   1. Never Married: 648   1. White:2480   1. < HS Grad      :268   2. Middle Atlantic   :3000  
# 1st Qu.:2004   1st Qu.:33.75   2. Married      :2074   2. Black: 293   2. HS Grad        :971   1. New England       :   0  
# Median :2006   Median :42.00   3. Widowed      :  19   3. Asian: 190   3. Some College   :650   3. East North Central:   0  
# Mean   :2006   Mean   :42.41   4. Divorced     : 204   4. Other:  37   4. College Grad   :685   4. West North Central:   0  
# 3rd Qu.:2008   3rd Qu.:51.00   5. Separated    :  55                   5. Advanced Degree:426   5. South Atlantic    :   0  
# Max.   :2009   Max.   :80.00                                                                    6. East South Central:   0  
# (Other)              :   0  

# jobclass               health      health_ins        wage       
# 1. Industrial :1544   1. <=Good     : 858   1. Yes:2083   Min.   : 20.09  
# 2. Information:1456   2. >=Very Good:2142   2. No : 917   1st Qu.: 85.38  
# Median :104.92  
# Mean   :111.70  
# 3rd Qu.:128.68  
# Max.   :318.34  




# Get training / test sets

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

dim(training)
# [1] 2102   10

dim(testing)
# [1] 898  10




# Feature plot ####

featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage,
            plot = "pairs")



