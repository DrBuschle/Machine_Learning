# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle

rm(list = ls())

# Week 2 lecture 5 - Basic preprocessing  ####


# Why preprocess?


library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# example for a very skewed variable
# most of the values are small, but there are outliers with huge values

hist(training$capitalAve, main = "", xlab = "ave. capital run length")

mean(training$capitalAve)
# [1] 5.049

sd(training$capitalAve)
# [1] 29.26

