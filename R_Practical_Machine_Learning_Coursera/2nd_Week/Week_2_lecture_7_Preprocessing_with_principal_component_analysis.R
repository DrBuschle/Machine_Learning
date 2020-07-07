# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle

rm(list = ls())

# Week 2 - Lecture 7 - Preprocessing with principal component analysis  ####

# Correlated predictors 

library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# leave out the 58th column, because this is the outcome (spam, nonspam)
M <- abs(cor(training[, -58]))
# cor: calculates the correlation between all these columns
# abs: absolute value 

# set selfcorrelation to 0, because we are not interested in this
diag(M) <- 0

# here you check which variables have a high correlation with each other

which(M > 0.8, arr.ind = T)

# row col
# num857  32  31
# num415  34  31
# telnet  31  32
# num415  34  32    <- this comes up in the example
# direct  40  32
# telnet  31  34
# num857  32  34    <- this comes up in the example
# direct  40  34
# num857  32  40
# num415  34  40

# in this case higher 0.8
# arr.ind: does not give the absoute position, but the position within the matrix





# Correlated predictions

names(spam)[c(34, 32)]
# [1] "num415" "num857"

# this is to check, if they are really highly correlated
plot(spam[, 34], spam [, 32])







# We could rotate the plot

# adding up the variables
X <- 0.71 * training$num415 + 0.71 * training$num857
# becomes the x-axis


# subtracting the variables
Y <- 0.71 * training$num415 - 0.71 * training$num857
# becomes the y-axis

# plotting
plot(X,Y)





# Principal components in R - prcomp

# make a small data set with the two values we found before to be highly correlated
smallSpam <- spam[, c(34, 32)]

# make principle component analysis
prComp <- prcomp(smallSpam)

# plot first against second principle component
plot(prComp$x[, 1], prComp$x[, 2])


# here you get a rotation matrix
# -> tells you how it is summing up the rotation matrix

prComp$rotation
#           PC1        PC2
# num415 0.7080625  0.7061498
# num857 0.7061498 -0.7080625

# the first PCA component explains the most variability -> in this case the sum of the two variables
# the second PCA component explains the most variability -> in this case the difference between the two variables





# PCA on SPAM data

# set the color for spam and nonspam
typeColor <- ((spam$type == "spam") * 1 + 1)

# calculate the PCA for all the variable expect spam
prComp <- prcomp(log10(spam[, -58] + 1))
# add 1 to make them look more gaussian and make sure there are not negative values for the log10 transformation 

# plot the first against the second PCA component
plot(prComp$x[, 1], prComp$x[, 2], col = typeColor, xlab = "PC1", ylab = "PC2")



# PC1 explains the most variation in the data
# it will be a quite complex thing, because it is build out of 57 variables

# PC2 explains the second most variation in the data
# PC3 ....

# -> this way you can reduce the size of your data set while still capturing a large amount of variation:
# this is a idea behind feature creation



# PCA with caret

# preprocces
preProc <- preProcess(log10(spam[, -58] + 1), method = "pca", pcaCom = 2)

# predict
spamPC <- predict(preProc, log10(spam[, -58] + 1))

# plot
plot(spamPC[, 1], spamPC[, 2], col = typeColor, xlab = "PC1", ylab = "PC2")





# Preprocessing with PCA
# install.packages("randomForest")
library(randomForest)

# preprocces
preProc <- preProcess(log10(training[, -58] + 1), method = "pca", pcaComp = 2)

# predict
trainPC <- predict(preProc, log10(training[, -58] + 1))

# fit the model
# modelFit <- train(training$type ~., method = "glm", data = trainPC)
# code does not work like this
modelFit <- train(x = trainPC, y = training$type, method = "glm")

# it related the training set variable "type" to the principle components: data = trainPC
