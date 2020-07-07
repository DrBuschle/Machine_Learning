# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle

rm(list = ls())

# Week 2 lecture 6 - Covariate creation  ####


# Two levels of covariate creation ####


# Level 2: Transforming tidy covaraibles

library(kernlab)
data(spam)

# square it
spam$capitalAveSq <- spam$capitalAve^2






# Load example data ####

library(ISLR)
library(caret)
data(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]



# Common covariates to add, dummy variables ####

# Basic idea - convert factor variable to indicator variables

table(training$jobclass)
# 1. Industrial 2. Information 
# 1062           1040 


# -> this is hard to use for prediction algorithms to use this information as it is 
# -> these are qualitative variable and have to be turned into quantitative variables


jobclass_dummies <- dummyVars(wage ~ jobclass, data = training)
# wage is the outcome
# jobclass is the predictor variable
# traing set is where the dummy variable are build

head(predict(jobclass_dummies, newdata = training))
# allpy a prediction on the "jobclass_dummies" and the training set
# use "newdata" instead of "data"


# jobclass.1. Industrial jobclass.2. Information
# 231655                      1                       0
# 86582                       0                       1
# 161300                      1                       0
# 155159                      0                       1
# 11443                       0                       1
# 376662                      0                       1




# Removing zero covariates ####

# Sometimes some of the variables have no variablity 
# e.g does an e-mail have a letter in it: yes/ no -> not a useful covariate, since nearly all mails do have a letter

# find variables with low variability, which are most likely no good predictors

nsv <- nearZeroVar(training, saveMetrics = T)
# saveMetrix = T: so you can see how it was calculated

nsv


#              freqRatio percentUnique zeroVar   nzv
# year        1.028736    0.33301618   FALSE FALSE
# age         1.014286    2.85442436   FALSE FALSE
# maritl      3.134783    0.23786870   FALSE FALSE
# race        8.705000    0.19029496   FALSE FALSE
# education   1.392562    0.23786870   FALSE FALSE
# region      0.000000    0.04757374    TRUE  TRUE
# jobclass    1.021154    0.09514748   FALSE FALSE
# health      2.462932    0.09514748   FALSE FALSE
# health_ins  2.289515    0.09514748   FALSE FALSE
# logwage     1.037037   19.31493815   FALSE FALSE
# wage        1.037037   19.31493815   FALSE FALSE

# percentUnique: percentage of unqiue values in the data set
# zeroVar: zero variable
# nzv: near zero variable


# -> region is a zero variable, subsequently a bad predictor and needs to be thrown out 





# Spline basis ####


