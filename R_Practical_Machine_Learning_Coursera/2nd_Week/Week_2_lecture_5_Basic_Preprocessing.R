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




# Standardization - preProcessfunction

# column 58 is the one we care about ("type"), it is the one about spam or not spam
preObj <- preProcess(training[ , -58], method = c("center", "scale"))
# method = c("center", "scale"): does what we did in the example before: 
# take the value, subtract the mean and this result gets divided by its standard deviation

trainCapAveS <- predict(preObj, training[ , -58])$capitalAve
mean(trainCapAveS)

# [1] -1.206e-17    <- same value like in the example before

sd(trainCapAveS)

# [1] 1       <- same value like in the example before


# the preObj can also be applied on the test set directly

testCapAveS <- predict(preObj, testing[ , -58])$capitalAve
mean(testCapAveS)
# [1] 0.01588

sd(testCapAveS)
# [1] 1.262





# Standardization - preProcessargument
# you can directly tell the train function to use the preProcess argument and specifiy it

set.seed(32343)
modelFit <- train(type ~., data = training, preProcess = c("center", "scale"), method = "glm")
modelFit

# Generalized Linear Model 
# 
# 3451 samples
# 57 predictor
# 2 classes: 'nonspam', 'spam' 
# 
# Pre-processing: centered (57), scaled (57) 
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 3451, 3451, 3451, 3451, 3451, 3451, ... 
# Resampling results:
#   
#   Accuracy  Kappa 
# 0.9203    0.8327




# Standardizing - Box-Cox transforms
# other kind of transformation

preObj <- preProcess(training[ , -58], method = c("BoxCox"))
# BoxCox takes continuous data and try to make them look like normal data
# they do that by estimating a specific set of parameters using maximum likelihood

trainCapAveS <- predict(preObj, training[ , -58])$capitalAve
par(mfrow = c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)


# at the original plot there was a huge bar at around 0






# Standardizing - Imputing data ("guess" NA values)

set.seed(13343)

# many algorithms have problems with missing values
# therefore we will introduce some, just to check how to handle this

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
training$capAve[selectNA] <- NA


# Impute (anrechnen) and standardize
preObj <- preProcess(training[ , -58], method = "knnImpute")
# knnImpute: k nearest neightbours Imputation
# finds the k nearest neighbours, which look most like the missing value and puts the average value to the missing one

# install.packages("RANN")

capAve <- predict(preObj, training[ , -58])$capAve


# Standardize true value
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth)) / sd(capAveTruth)



# Standardize - Imputing data

# all value (including imputed values) - true values
quantile(capAve - capAveTruth)

# 0%        25%        50%        75%       100% 
# -12.541920  -0.013119  -0.008136   0.002258   4.588768 

# around 50 % the difference is close to zero, here it worked well
# at 0 % and 100 % the differences are much larger, here it didn't work too well


# only imputed values - true values
quantile((capAve - capAveTruth)[selectNA])
# 0%        25%        50%        75%       100% 
# -12.541920  -0.017931  -0.005889   0.017628   1.178953 

# -> the value should be more variable


# only NOT imputed values - true values
quantile((capAve - capAveTruth)[!selectNA])

# 0%       25%       50%       75%      100% 
# -0.017023 -0.012991 -0.008192  0.001791  4.588768 

# the values should be closer to each other


