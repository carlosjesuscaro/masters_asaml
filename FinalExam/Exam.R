# Author: Carlos Jesus Caro
# Email: carlos.jesus-caro@edu.dsti.institute

##############################################################################
# Exercise 1
##############################################################################

# Importing the data
data <- read.table('./FinalExam/data1.txt', header = FALSE)
head(data)
class(data$V1)

# Option 1
##########

# Plotting the histogram
hist(data$V1, freq = FALSE)
# Note: Based on the histogream, it looks like an Uniform distribution

# Applying the Kolmogorov-Smirnov test
ks.test(data$V1, punif, min(data$V1), max(data$V1))
# Note: p-value: 0.783 ==> H0 is not rejected, data$V1 seems to be a uniform
# random variable

# Option 2
##########

# Calculating the time arrival between 2 points
data_diff <- diff(data$V1)

# Histogram of the time difference between 2 points from data$V1
hist(data_diff, freq = FALSE)
# Note: Based on the histogream, it looks like an Exponential distribution

c
ks.test(data_diff, pexp, 1 / mean(data_diff))
# Note: p-value: 0.5568 ==> We accept H0, the difference between 2 points of data$V1
# is an exponential random variable

# Calculating lambda
lambda_hat <- 1 / mean(data_diff)
lambda_hat

##############################################################################
# Exercise 2
##############################################################################

# Importing the data
uk_train <- read.table('./FinalExam/ukcomp1_r.dat', header = TRUE)
uk_test <- read.table('./FinalExam/ukcomp2_r.dat', header = TRUE)

# Exploring the training data set
head(uk_train)
class(uk_train)
dim(uk_train)

# Exploring the test data set
head(uk_test)
class(uk_test)
dim(uk_test)

# Note: All variables are numeric

# Checking the correlation
cor(uk_train)
# Note: There is no indication that a varibale can be supressed from
# the correlatin analysis

# Building a linear model
lm_model <- lm(RETCAP~., data = uk_train)
summary(lm_model)
#  Note: Wihtout validating the noise, we can only consider the value
# coefficients since they are calculated with the least squared method
# and the adjusted R^2 value. Additional interpretations require the
# validation of the noise

# Validating the noise
# Step 1: Studiantized residuals
st_res <- rstudent(lm_model)
# Step 2: Histogram analysis
hist(st_res, freq = FALSE)
# Note: it seems to be Gaussian but it is not conclusive
# Step 3: QQ plot
qqnorm(st_res)
# Note: it seems to be ok for a Gaussian noise
# Step 4: checking homoscedasticity
plot.res <- function(x,y,title=" ")
{
  plot(x,y,col='blue',ylab='Residuals',xlab='fitted values',main=title)
  abline(h=0,col='green')
}
plot.res(predict(lm_model), st_res)
# Note: it looks ok for homscedasticity
# Applying the Shapiro test
shapiro.test(st_res)
# Notes: p-value = 0.167 ==> The residuals are Gaussian

# Variable selection
library(MASS)







