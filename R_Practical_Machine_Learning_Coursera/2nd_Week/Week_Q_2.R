# Coursera: Prac Mac lear

rm(list = ls())

# Week 2 - Q2  ####


# 1 ####

# install.packages('AppliedPredictiveModeling')
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

# Answer:

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


# -> what the question teaches you is, that you need to have your variable to be predicted in one data frame together with your predictors






# 2 ####

rm(list = ls())

# Load the cement data using the commands:

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)

inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function
# in the Hmisc package useful for turning continuous covariates into factors). 
# What do you notice in these plots? 



# -> totally unclear what the question is about. One can only guess based on the results


plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = "red", xlab = "Index", ylab = "Compressive Strenght")

library(Hmisc) 

cut_ComStrength <- cut2(training$Cement, g = 3) 
cut_BlastFurnaceSlag <- cut2(training$BlastFurnaceSlag, g = 3) 
cut_FlyAsh <- cut2(training$FlyAsh, g = 3) 
cut_Water <- cut2(training$Water, g = 3) 
cut_Superplasticizer <- cut2(training$Superplasticizer, g = 3) 
cut_CoarseAggregate <- cut2(training$CoarseAggregate, g = 3) 
cut_FineAggregate <- cut2(training$FineAggregate, g = 3) 
cut_Age <- cut2(training$Age, g = 3) 
cut_CompressiveStrength <- cut2(training$CompressiveStrength, g = 3) 

plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_ComStrength, xlab = "Index", ylab = "Compressive Strenght")
plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_BlastFurnaceSlag, xlab = "Index", ylab = "Compressive Strenght")
plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_FlyAsh, xlab = "Index", ylab = "Compressive Strenght")
plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_Water, xlab = "Index", ylab = "Compressive Strenght")
plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_Superplasticizer, xlab = "Index", ylab = "Compressive Strenght")
plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_CoarseAggregate, xlab = "Index", ylab = "Compressive Strenght")
plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_FineAggregate, xlab = "Index", ylab = "Compressive Strenght")
plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_Age, xlab = "Index", ylab = "Compressive Strenght")
plot(1:nrow(training), training$CompressiveStrength, pch = 19, col = cut_CompressiveStrength, xlab = "Index", ylab = "Compressive Strenght")

# -> Answer: There is a non-random pattern in the plot of the outcome versus index that does not appear
#            to be perfectly explained by any predictor suggesting a variable may be missing




# 3 ####

rm(list = ls())

# Load the cement data using the commands:

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log 
# transform to try to make the data more symmetric. Why would that be a poor choice for this variable?


# -> again, the question is not clear. What data set are we supposed to work with?
# Probably training, but the info is missing

hist(log(training$Superplasticizer + 1))

# Answer: There are a large number of values that are the same and even if you took the log(Superplasticizer + 1)
#         they would still all be identical so the distribution would not be symmetric





# 4 ####

rm(list = ls())

# Load the Alzheimer's disease data using the commands:

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function
# from the caret package. Calculate the number of principal components needed to 
# capture 80% of the variance. How many are there?

# Find all the predictor variables in the training set that begin with IL.:
names(training)[grep("^IL", names(training))]

# Perform principal components on these variables with the preProcess() function
# from the caret package.:

# take diagnosis and all IL columns
ILs <- training[, grep("^IL", names(training))]


# Calculate the number of principal components needed to 
# capture 80% of the variance. How many are there?:

preProc <- preProcess(ILs[, grep("^IL", names(ILs))], method = "pca", thresh = 0.8) 
preProc

# Created from 251 samples and 12 variables
# 
# Pre-processing:
#   - centered (12)
# - ignored (0)
# - principal component signal extraction (12)
# - scaled (12)
# 
# PCA needed 7 components to capture 80 percent of the variance

preProc$rotation

# PC1           PC2           PC3          PC4         PC5          PC6         PC7
# IL_11         -0.06529786  0.5555956867  0.2031317937 -0.050389599  0.73512798 -0.102014559  0.20984151
# IL_13          0.27529157  0.3559427297 -0.0399010765  0.265076920 -0.25796332 -0.068927711  0.58942516
# IL_16          0.42079000  0.0007224953  0.0832211446 -0.082097273  0.04435883 -0.007094672 -0.06581741
# IL_17E        -0.01126118  0.5635958176  0.3744707126  0.302512329 -0.38918707  0.221149380 -0.46462692
# IL_1alpha      0.25078195 -0.0687043488 -0.3008366900  0.330945942  0.16992452  0.742391473  0.12787035
# IL_3           0.42026485 -0.0703352892 -0.1049647272 -0.065352774  0.02352819 -0.165587911 -0.09006656
# IL_4           0.33302031  0.0688495706 -0.1395450144  0.165631691 -0.14268797 -0.297421293  0.19661173
# IL_5           0.38706503 -0.0039619980  0.0005616126 -0.224448981  0.08426042  0.153835977 -0.16425757
# IL_6           0.05398185 -0.4248425653  0.6090821756  0.417591202 -0.00165066 -0.166089521  0.21895103
# IL_6_Receptor  0.21218980  0.1005338329  0.2920341087 -0.659953479 -0.29654048  0.138000448  0.22657846
# IL_7           0.32948731  0.0806070090 -0.1966471906  0.165544952  0.11373532 -0.405698338 -0.42065832
# IL_8           0.29329723 -0.1883039842  0.4405255221  0.002811187  0.28608600  0.184321013 -0.14833779


# Answer: there are 7



# 5 ####

rm(list = ls())

# Load the Alzheimer's disease data using the commands:

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Create a training data set consisting of only the predictors with variable names beginning 
# with IL and the diagnosis. Build two predictive models, one using the predictors as they
# are and one using PCA with principal components explaining 80% of the variance in the predictors.
# Use method="glm" in the train function. 

# Create a training data set consisting of only the predictors with variable names beginning 
# with IL and the diagnosis.
training_diagnosis_With_ILs <- training[, grep("diagnosis|^IL", names(training))]
testing_diagnosis_With_ILs <- testing[, grep("diagnosis|^IL", names(testing))]

# Build two predictive models, 

# training: using the predictors as they are: 

modelFit_common <- train(x = training_diagnosis_With_ILs[, -1], y = training_diagnosis_With_ILs$diagnosis, method = "glm") 
# alternative way of writing, with same result
modelFit_common <- train(diagnosis ~., method = "glm", data = training_diagnosis_With_ILs)


# predict on testing set
predict_testing_common <- predict(modelFit_common, newdata = testing_diagnosis_With_ILs) 


## get the confustion matrix for the common method
ConMat_common <- confusionMatrix(predict_common, testing_diagnosis_With_ILs$diagnosis)
ConMat_common$overall[1]





# and one using PCA with principal components explaining 80% of the variance in the predictors.
# Use method="glm" in the train function. 

# preprocces 
preProc_PCA <- preProcess(training_diagnosis_With_ILs[, -1], method = "pca", thresh = 0.8) 

# predict for training
predict_for_train_PCA <- predict(preProc_PCA, training_diagnosis_With_ILs[, -1]) 

# training: using the PCA predictors: 

modelFit_PCA <- train(x = predict_for_train_PCA, y = training_diagnosis_With_ILs$diagnosis, method = "glm") 


# testing:
# predict for testing
predict_for_test_PCA <- predict(preProc_PCA, testing_diagnosis_With_ILs[, -1]) 

ConMat_PCA <- confusionMatrix(testing_diagnosis_With_ILs$diagnosis, predict(modelFit_PCA, predict_for_test_PCA))
ConMat_PCA$overall[1]


