# Coursera: Practical Machine learning
# Script from course commented by Alexander Buschle


# Week 2 lecture 4 - Plotting predictors ####

# install.packages('ISLR')

library(ISLR)
library(ggplot2)
library(caret)

data(Wage)

summary(Wage)

# year           age                    maritl           race                   education                     region               jobclass               health      health_ins      logwage          wage      
# Min.   :2003   Min.   :18.0   1. Never Married: 648   1. White:2480   1. < HS Grad      :268   2. Middle Atlantic   :3000   1. Industrial :1544   1. <=Good     : 858   1. Yes:2083   Min.   :3.00   Min.   : 20.1  
# 1st Qu.:2004   1st Qu.:33.8   2. Married      :2074   2. Black: 293   2. HS Grad        :971   1. New England       :   0   2. Information:1456   2. >=Very Good:2142   2. No : 917   1st Qu.:4.45   1st Qu.: 85.4  
# Median :2006   Median :42.0   3. Widowed      :  19   3. Asian: 190   3. Some College   :650   3. East North Central:   0                                                             Median :4.65   Median :104.9  
# Mean   :2006   Mean   :42.4   4. Divorced     : 204   4. Other:  37   4. College Grad   :685   4. West North Central:   0                                                             Mean   :4.65   Mean   :111.7  
# 3rd Qu.:2008   3rd Qu.:51.0   5. Separated    :  55                   5. Advanced Degree:426   5. South Atlantic    :   0                                                             3rd Qu.:4.86   3rd Qu.:128.7  
# Max.   :2009   Max.   :80.0                                                                    6. East South Central:   0                                                             Max.   :5.76   Max.   :318.3  
# (Other)              :   0                                                                          



# Get training/test sets 

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain, ]
test <- Wage[-inTrain, ]
dim(training); dim(testing)

# [1] 2102   11
# [1] 1150   58



# Feature plot (caret package) 

featurePlot(x = training[ , c("age", "education", "jobclass")],
              y = training$wage,
            plot = "pairs")

# y: the variable we care about -> wage (Lohn)
# plot = "pairs": plot the vairables in pairs

# The plot explained: in the middle you the the different variables:
# age, education, jobclass, and y (wage)
# when you connect age with y by two straight lines you will end in the
# field where their information is combined




# Qplot (ggplot2 package) 

qplot(age, wage, data = training)
qplot(training$age, training$wage)



# Qplot with color (ggplot2 package) 

qplot(age, wage, color = jobclass, data = training)


# Add regression smoothers (ggplot2 package)

my_plot <- qplot(age, wage, color = education, data = training)
my_plot + geom_smooth(method = 'lm', formula = y ~ x)

# geom_smooth: adds a linear smoother to the data
# -> fits a linear model for every education class



# cut2, making factirs (Hmisc package)

library(Hmisc)

cutWage <- cut2(training$wage, g = 3)
# g: tells in how many groups the data should be split off
# it will break the data sets up into factors based on quantile groups
table(cutWage)

# quantile groups: from 1st: 20.1 to 93, 2nd: 93.0 to 119, 3rd: 119.4 to 318

# [ 20.1, 93) [ 93.0,119) [119.4,318] 
# 710         730         662 



# Boxplots with cut2

my_boxplot <- qplot(cutWage, age, data = training, fill = cutWage,
                    geom = c("boxplot"))
my_boxplot

# -> this can make it easier to see trends, here you see a trend that age and wage has
# a connection



# Boxplot with points overlayed

my_boxplot_jittered <- qplot(cutWage, age, data = training, fill = cutWage,
                    geom = c("boxplot", "jitter"))

# geom = c("boxplot", "jitter")): say to plot boxplot and points to the same plot

my_boxplot

library(grid)
library(gridExtra)

# arranges several plots next to each other
grid.arrange(my_boxplot, my_boxplot_jittered, ncol = 2)


