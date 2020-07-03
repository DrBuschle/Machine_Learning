# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle


# Week 2 lecture 3 - Training options ####

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


# SPAM Example: Training options


# split up your dataset to training and testing set
# compute vector for this task

# split the data on 'type'
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F) # p: use 75 % of data to train the model and 25 % to test it

# split up your data
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]


# train your algorithm
modelFit <- train(type ~., data = training, method = "glm")
# on vector training
# use the method "glm"


# Training options
args(train.default)

?train

# here come the argument of the function
# train(
#   x,
#   y,
#   method = "rf",
#   preProcess = NULL,    <- do some preprocessing
#   ...,
#   weights = NULL,       <- set up weights, is important when you have many more of one type than from another
#   metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
#       <- by default for factor variables and categorial variables the default is "Accuracy"
#       <- for continuous data the metric should be RMSE (rout mean square error)
#   maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE"), FALSE, TRUE),
#   trControl = trainControl(),   <- will be explained later
#   tuneGrid = NULL,
#   tuneLength = ifelse(trControl$method == "none", 1, 3)
# )





# trainControl

# lets you be much more precise about how you train models

args(trainControl)

# function (method = "boot",  <- you can choose between bootstrapping and crossvalidation
#           number = ifelse(grepl("cv", method), 10, 25), 
#           repeats = ifelse(grepl("[d_]cv$", method), 1, NA),    <- how many times to repeat
#           p = 0.75,     <- set percentage of the training set
#           search = "grid",
#           initialWindow = NULL,  <- for time course data you can tell the number of time points / units
#           horizon = 1,
#           fixedWindow = TRUE, skip = 0, verboseIter = FALSE, returnData = TRUE,
#           returnResamp = "final", 
#           savePredictions = FALSE,  <- tell it to save the predictions of every iteration
#           classProbs = FALSE,
#           summaryFunction = defaultSummary,  <- different summaries, compared to default
#           selectionFunction = "best",
#           preProcOptions = list(thresh = 0.95,   <- set pre processing options
#           ICAcomp = 3, k = 5,
#           freqCut = 95/5, uniqueCut = 10, cutoff = 0.9), sampling = NULL,
#           index = NULL, indexOut = NULL, indexFinal = NULL, timingSamps = 0,
#           predictionBounds = rep(FALSE, 2),   <- set prediction bound
#           seeds = NA,   <- set seeds for all the different resampling layers
#           adaptive = list(min = 5,
#           alpha = 0.05, method = "gls", complete = TRUE), trim = FALSE,
#           allowParallel = TRUE        <- here you can use multiple core and parallelization)
# NULL





# Seed example

set.seed(1235)
modelFit2 <- train(type ~., data = training, method = "glm")
modelFit2


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
#   Accuracy  Kappa 
# 0.9179    0.8272


