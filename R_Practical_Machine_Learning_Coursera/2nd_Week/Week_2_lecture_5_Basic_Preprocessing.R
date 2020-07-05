# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle

rm(list = ls())

# Week 2 lecture 5 - Basic preprocessing  ####


# Why preprocess?


library(caret)
library(kernlab)
data(spam)

set.seed(1235)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# example for a very skewed variable
# most of the values are small, but there are outliers with huge values

hist(training$capitalAve, main = "", xlab = "ave. capital run length")

mean(training$capitalAve)
# [1] 5.074

sd(training$capitalAve)
# [1] 29.61






# Standardizing - training set

trainCapAve <- training$capitalAve

# typical standarization: take the value, subtract the mean and this result gets divided by its standard deviation
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)

mean(trainCapAveS)
# [1] -1.206e-17    <- mean gets very close to 0

sd(trainCapAveS)
# [1] 1   <- standard deviation gets 1

# -> strong reduction of variability and standarize the variables





# Standardizing - test set
# WHEN YOU APPLY a standardization to a training set, you also have to do that for the test set!!!!
# BUT YOU HAVE TO use the values from the TRAINING SET for standadizing the TEST SET!!!!


testCapAve <- testing$capitalAve

# typical standarization: take the value, subtract the mean and this result gets divided by its standard deviation
testCapAveS <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)

mean(testCapAveS)
# [1] 0.01588    <- mean gets in the direction of 0, but most likley not too close

sd(testCapAveS)
# [1] 1.262     <- standard deviation gets close to 1, but most likley not too close





