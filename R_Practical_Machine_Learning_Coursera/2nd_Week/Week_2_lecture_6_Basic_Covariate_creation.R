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

# you can do linear regression, but you can also do that on curvy lines

library(splines)

bsBasis <- bs(training$age, df = 3)
# bs(): creates a polynominal variable
# df: degrees of freedom; 3 = third degree polynominal 
# take only one variable: here age

bsBasis

#           1            2            3
# [1,] 0.00000000 0.0000000000 0.000000e+00
# [2,] 0.23685006 0.0253767916 9.063140e-04
# [3,] 0.41633799 0.3211750193 8.258786e-02
# [4,] 0.43081384 0.2910904300 6.556091e-02
# [5,] 0.36252559 0.3866939680 1.374912e-01
# [6,] 0.30633413 0.4241549461 1.957638e-01
# [7,] 0.42415495 0.3063341278 7.374710e-02
# ...

# 1: actual age, but they are scaled for computational purposes
# 2: age^2 (squared): to find a quadratic relationship between age and outcome
# 3: age^3 (cubed): to find a cubic relationship between age and outcome



