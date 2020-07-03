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

# now returnTrain = FALSE
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)

sapply(folds, length) 

# Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10 
#460    461    460    459    461    459    460    460    461    460 

folds[[1]][1:10]
# [1] 24 27 32 40 41 43 55 58 63 68


