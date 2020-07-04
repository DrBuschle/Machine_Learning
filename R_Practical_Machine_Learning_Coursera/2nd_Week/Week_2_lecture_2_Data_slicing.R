# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle


# Week 2 lecture 2 - Data slicing####

# same start as Week 2 lecture 1

# get rid of all old data
rm(list = ls())

# SPAM Example: Data splitting


# install.packages("caret")
# install.packages('e1071')
library(caret)
library(kernlab)
library(e1071)

# load the spam data
data(spam)


# SPAM Example: Data Splitting


# split up your dataset to training and testing set
# compute vector for this task

# split the data on 'type'
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F) # p: use 75 % of data to train the model and 25 % to test it

# split up your data
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)



# SPAM Example: K-fold

# set a seed to make the data reproducible
set.seed(32323)

folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)
# k: number of folds to create
# list: "list = TRUE" tells the program to give back the values of the folds as a list
# returnTrain: "returnTrain = TRUE" tells the program to return the training set

# check the length of the created folds
sapply(folds, length) 

# Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10 
# 4141   4140   4141   4142   4140   4142   4141   4141   4140   4141 

# access the the data of the different folds

folds[[1]][1:10]  # check fold 1 and its 10 first elements

#  [1]  1  2  3  4  5  6  7  8  9 10





# SPAM Example: Return test

set.seed(32323)

# now returnTrain = FALSE -> it will return the TEST set!!!!!
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)

sapply(folds, length) 

# Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10 
#460    461    460    459    461    459    460    460    461    460 

folds[[1]][1:10]
# [1] 24 27 32 40 41 43 55 58 63 68





# SPAM Example: Resampling

# you sample with replacement -> you can get value multiple times

set.seed(32323)

# here you resample
folds <- createResample(y = spam$type, times = 10, list = TRUE)
# times: tells the algorithm how often to resample

sapply(folds, length) 

# Resample01 Resample02 Resample03 Resample04 Resample05 Resample06 Resample07 Resample08 Resample09 Resample10 
# 4601       4601       4601       4601       4601       4601       4601       4601       4601       4601 

folds[[1]][1:10]
# [1]  1  2  3  3  3  5  5  7  8 12





# SPAM Example: Time Slices 

set.seed(32323)

# time vector of thousand units
tme <- 1:1000

# here you create your time slices
folds <- createTimeSlices(y = tme, initialWindow = 20 , horizon = 10)
# initialWindow: "initialWindow = 20" tells that the slices will have a size of 20 units
# horizon: "horizon = 10" tells that you want to predict the following 10 units


# folds gets split up to train and test set automatically
names(folds)
# [1] "train" "test" 

# here is the result of the "initialWindow = 20" option
folds$train[[1]]
#  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

# here is the result of the "horizon = 10" option
folds$test[[1]]
#  [1] 21 22 23 24 25 26 27 28 29 30

