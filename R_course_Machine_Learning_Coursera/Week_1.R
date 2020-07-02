# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle

# Week 1 ####
# Example 1 ####

rm(list = ls())
#install.packages("kernlab")


library(kernlab)
data(spam)

plot(density(spam$your[spam$type == "nonspam"]), col = "blue", main = '', xlab = "Frequency of 'your'")
lines(density(spam$your[spam$type == "spam"]), col = "red")

# define a cutoff, with which you define if a mail is spam or not depending on the usage of the word "your"
abline(v = 0.5, col = "black")


prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
prediction_table <- table(prediction, spam$type) / length(spam$type)


# accuracy

# really nonspam + really spam
round(prediction_table[1,1] + prediction_table[2,2], 3)
# 0.751




# Example 2 ####

library(kernlab); data(spam); set.seed(333)

smallSpam <- spam[sample(dim(spam)[1], size = 10), ]
spamLabel <- (smallSpam$type == "spam") * 1 + 1
plot(smallSpam$capitalAve, col = spamLabel, pch = 16)


# apply rules on your dataset

# Rule 1

rule_1 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.7] <- "spam"
  prediction[x < 2.40] <- "nonspam"
  prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
  prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
  return(prediction)
  }

# transform to a table

table(rule_1(smallSpam$capitalAve), smallSpam$type)

# -> this is a perfect prediction for the sample data set


# Rule 2

rule_2 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x <= 2.8] <- "nonspam"
  return(prediction)
}

table(rule_2(smallSpam$capitalAve), smallSpam$type)

# there is a small error in the prediction for the sample data set



# apply rules to big data sets

## rule 1 (very accurate for test set)
table_rule_1 <- table(rule_1(spam$capitalAve), spam$type) 
rownames(table_rule_1) <- c("predict_nonspam", "predict_spam")
table_rule_1    


## rule 2 (less accurate for test set)
table_rule_2 <- table(rule_2(spam$capitalAve), spam$type)
rownames(table_rule_2) <- c("predict_nonspam", "predict_spam")
table_rule_2 



# check the accuracy
# check how often spam is really spam and nonspam is really nonspam and then sum these up

# rule 1
sum(rule_1(spam$capitalAve) == spam$type)

# rule 2
sum(rule_2(spam$capitalAve) == spam$type)


