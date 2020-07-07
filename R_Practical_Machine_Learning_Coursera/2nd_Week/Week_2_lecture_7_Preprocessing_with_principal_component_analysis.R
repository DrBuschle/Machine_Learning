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
X <- 1.1 * training$num415 + 1.1 * training$num857
# becomes the x-axis


# subtracting the variables
Y <-1.1 * training$num415 - 1.1 * training$num857
# becomes the y-axis

# plotting
plot(X,Y)


