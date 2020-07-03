# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle

# Week 2 ####

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


# split up your dataset to training and testing set
# compute vector for this task

# split the data on 'type'
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F) # p: use 75 % of data to train the model and 25 % to test it

# split up your data
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(testing)



# set a seed to make your data reproducable
set.seed(32343)

# train(type: here you predict 'type', 
# ~.: the ~(tilde). tells the algorithm to use all the other variables to for the prediction
# data = training: on which dataset to build the training model on 
# method = "glm": tells you which algorithm to use for prediction


# train your model
modelFit <- train(type ~., data = training, method = "glm")
modelFit


# output

# Generalized Linear Model 
# 
# 3451 samples  <- takes all your samples
# 57 predictor  <- and all your predictors (columns with infos)
# 2 classes: 'nonspam', 'spam'  <- from this 2 classes are build, either 'spam' or 'nonspam'
# 


# No pre-processing
# Resampling: Bootstrapped (25 reps) 

# <- it check different models and checks which one works best
# <- in this case it used "resampling" and bootstrapping with 25 replicated
# <- corrects for the bias, which might come from bootstrap sampling

# Summary of sample sizes: 3451, 3451, 3451, 3451, 3451, 3451, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.9183707  0.8285283
# 
# > modelFit
# Generalized Linear Model 
# 
# 3451 samples
# 57 predictor
# 2 classes: 'nonspam', 'spam' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 3451, 3451, 3451, 3451, 3451, 3451, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.9183707  0.8285283


# output of the model

modelFit$finalModel
# -> this will give you the fitted values for each predictor you got for the "glm" model 


# now you can use your model ("modelFit") to predict on new samples

predictions <- predict(modelFit, newdata = testing)
predictions

# predictions can be used to check if your model fit works well or not
# to check if your model fit works well, use the confusionMatrix

# your pass your predictions ("predictions") and the real values from your testing set (testing$type)
# to build a table and it calculates the accuracy (TP + TN) for you

confusionMatrix(predictions, testing$type)
confusionMatrix(predictions, testing$type)


# outcome

# Confusion Matrix and Statistics
# 
#             Reference
# Prediction nonspam spam
# nonspam     675   46
# spam         22  407
# 
# Accuracy : 0.9409         <- (675+407)/(675+407+22+46) 
# 95% CI : (0.9256, 0.9538)
# No Information Rate : 0.6061          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.875           
# 
# Mcnemar's Test P-Value : 0.005285        
# 
# Sensitivity : 0.9684          <- 675/(675+22)
# Specificity : 0.8985          <- 407/(407+46)
# Pos Pred Value : 0.9362       <- 675/(675+46)   
# Neg Pred Value : 0.9487       <- 407/(407+22)
# Prevalence : 0.6061          
# Detection Rate : 0.5870          
# Detection Prevalence : 0.6270          
# Balanced Accuracy : 0.9334          
# 
# 'Positive' Class : nonspam         


