# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle

rm(list = ls())

# Week 2 - Lecture 8 - Prediction with Regression  ####

# Example: Old faithful eruptions ####

library(caret)
data(faithful)
set.seed(333)

inTrain <- createDataPartition(y = faithful$waiting, p = 0.5, list = FALSE)
trainFaith <- faithful[inTrain, ]
testFaith <- faithful[-inTrain, ]
head(trainFaith)

#      eruptions waiting
# 1     3.600      79
# 3     3.333      74
# 5     4.533      85
# 6     2.883      55
# 7     4.700      88
# 8     3.600      85




# Eruption duration versus waiting time ####
plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = "blue", xlab = "Waiting", ylab = "Duration")





# Fit a linear model ####

lm1 <- lm(eruptions ~ waiting, data = trainFaith)
# outcome (eruptions) is what you want to predict
# ~: the tilde sign says that you predict the outcome as a function of everything right of the tilde
# data: tells the programm which data to use

summary(lm1)


# Call:
#   lm(formula = eruptions ~ waiting, data = trainFaith)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.26990 -0.34789  0.03979  0.36589  1.05020 
# 
# Coefficients:
#                 | 
#                 V   <-------- the estimates are the important points when it is about prediction
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.792739   0.227869  -7.867 1.04e-12 ***
#   waiting      0.073901   0.003148  23.474  < 2e-16 ***
#
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.495 on 135 degrees of freedom
# Multiple R-squared:  0.8032,	Adjusted R-squared:  0.8018 
# F-statistic:   551 on 1 and 135 DF,  p-value: < 2.2e-16



# Easier to read summary is this:
lm1

# Call:
#   lm(formula = eruptions ~ waiting, data = trainFaith)
# 
# Coefficients:
#   (Intercept)      waiting  
# -1.7927       0.0739  


# (Intercept) in Estimate is the b0 constant
# waiting is the b1 constant






# Model fit ####
plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = "blue", xlab = "Waiting", ylab = "Duration")
lines(trainFaith$waiting, lm1$fitted.values, lwd = 3)





# Predict a new value ####
coef(lm1)[1] + coef(lm1)[2] * 80
# or
lm1$coefficients[1] + lm1$coefficients[2] * 80

# (Intercept) 
# 4.119307 

# automated
newdata <- data.frame(waiting = 80)
predict(lm1, newdata)



# Plot predictions – training and test ####

par(mfrow = c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = "blue", xlab = "Waiting", ylab = "Duration")
lines(trainFaith$waiting, predict(lm1), lwd = 3)

plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = "blue", xlab = "Waiting", ylab = "Duration")
lines(testFaith$waiting, predict(lm1, newdata = testFaith), lwd = 3)





# Get training set/ test set errors ####

# Calculate RMSE on training
sqrt(sum((lm1$fitted.values - trainFaith$eruptions)^2))
# [1] 5.75186


# Calculate RMSE on test
sqrt(sum((predict(lm1, newdata = testFaith) - testFaith$eruptions)^2))
# [1] 5.838559





# Prediction intervals ####

par(mfrow = c(1,1))
pred1 <- predict(lm1, newdata = testFaith, interval = "prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = "blue")
matlines(testFaith$waiting[ord], pred1[ord, ], type = "l", col = c(1,2,2), lty = c(1,1,1), lwd = 3)




# Same process with caret

modFit <- train(eruptions ~ waiting, data = trainFaith, method = "lm")
# outcome: eruptions
# predictor: wainting
# data: data set, on which to build it on
# method: linear modelling

summary(modFit$finalModel)

# Call:
#   lm(formula = .outcome ~ ., data = dat)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.26990 -0.34789  0.03979  0.36589  1.05020 
# 
# Coefficients:
#                   |
#                   V       <- here is about the same result like for our hand made one
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.792739   0.227869  -7.867 1.04e-12 ***
#   waiting      0.073901   0.003148  23.474  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.495 on 135 degrees of freedom
# Multiple R-squared:  0.8032,	Adjusted R-squared:  0.8018 
# F-statistic:   551 on 1 and 135 DF,  p-value: < 2.2e-16


# also interesting 
modFit

# Linear Regression 
# 
# 137 samples
# 1 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 137, 137, 137, 137, 137, 137, ... 
# Resampling results:
#   
#   RMSE       Rsquared   MAE      
# 0.5043234  0.7976798  0.4092342
# 
# Tuning parameter 'intercept' was held constant at a value of TRUE



