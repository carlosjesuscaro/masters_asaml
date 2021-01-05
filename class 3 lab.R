# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-05-30

library(dplyr)
library(ggplot2)
library(reshape)

# Uploading the data
Y1 = read.table('Y1.txt')
Y2 = read.table('Y2.txt')
Y3 = read.table('Y3.txt')
Y4 = read.table('Y4.txt')
A1 = read.table('A1.txt')
A3 = read.table('A3.txt')
A4 = read.table('A4.txt')

# Case 1
#========
# Understanding the data
summary(Y1)
# Plotting Y1
ggplot(Y1, aes(x=V1)) + geom_histogram() + ggtitle('Y1$V1 Histogram')
ggplot(Y1, aes(x=seq(1:50), y=V1)) + geom_line() + ggtitle('Y1$V1')
# Plotting A1
A11 <- A1 %>% mutate(index = seq(1:50))
A11 <- melt(A11, id.vars ='index', variable.names = 'series')
ggplot(A11, aes(x=value)) + geom_histogram() + facet_wrap(~ variable)
# Checking the correlation among the explanatory variables
cor(A1)
# Linear models
print('Linear Model 1')
L1 = lm(as.matrix(Y1)~.,data = as.data.frame(A1))
summary(L1)
plot(L1)
# verifying the normality of the residuals
hist(L1$residuals, freq = FALSE)
# Shapiro Wilk test
shapiro.test(L1$residuals)
# Kolmogorov test for normality
ks.test(L1$residuals, "pnorm", mean = mean(L1$residuals),
        sd = sd(L1$residuals))
# Conclusion
# This is not a multi linear model as the adjusted R2 is very low
# plus the residuals are not in the Gaussian distribution. We can
# also see that there is no correlation among the variable in A1

# Case 2
#========
# Understanding the data
summary(Y2)
# Plotting Y2
ggplot(Y2, aes(x=V1)) + geom_histogram() + ggtitle('Y2$V1 Histogram')
ggplot(Y2, aes(x=seq(1:50), y=V1)) + geom_line() + ggtitle('Y1$V1')
# Linear models
print('Linear Model 2')
L2 = lm(as.matrix(Y2)~.,data = as.data.frame(A1))
summary(L2)
plot(L2)
# verifying the normality of the residuals
hist(L2$residuals, freq = FALSE)
# Shapiro Wilk test
shapiro.test(L2$residuals)
# Kolmogorov test for normality
ks.test(L2$residuals, "pnorm", mean = mean(L2$residuals),
        sd = sd(L2$residuals))
# Conclusion
# This is a much better case where we can say that Y2 can be modeled
# through a multi-linear regression model. The residuals are Gaussian
# so we can accept the Fisher and Student test
# The R2 (and Adjusted R2) show a good performance where the
# most critical coefficients are V1 and V5. IMPORTANT to note that
# since there is no correlation among the explanatory variables,
# then we can eliminate the explanatory variables based on the
# individual Student test

# Case 3
#========
# Understanding the data
summary(Y3)
# Plotting Y3
ggplot(Y3, aes(x=V1)) + geom_histogram() + ggtitle('Y3$V1 Histogram')
ggplot(Y3, aes(x=seq(1:50), y=V1)) + geom_line() + ggtitle('Y3$V1')
# Plotting A3
A33 <- A3 %>% mutate(index = seq(1:50))
A33 <- melt(A33, id.vars ='index', variable.names = 'series')
ggplot(A33, aes(x=value)) + geom_histogram() + facet_wrap(~ variable)
# Checking the correlation among the explanatory variables
cor(A3)
# Linear models
print('Linear Model 3')
L3 = lm(as.matrix(Y3)~.,data = as.data.frame(A3))
summary(L3)
plot(L3)
# verifying the normality of the residuals
hist(L3$residuals, freq = FALSE)
# Shapiro Wilk test
shapiro.test(L3$residuals)
# Kolmogorov test for normality
ks.test(L3$residuals, "pnorm", mean = mean(L3$residuals),
        sd = sd(L3$residuals))
# Conclusion
# Y3 is well modelled by a simple linear regression since the main
# variable is V5 (only 1). However, we cannot eliminate V1 nor V6
# due to the high level correlation among them.In addition,
# we can confirm that the residuals of the model are Gaussian
# (R2 score is great)

# Case 4
#========
# Understanding the data
summary(Y4)
# Plotting Y3
ggplot(Y4, aes(x=V1)) + geom_histogram() + ggtitle('Y4$V1 Histogram')
ggplot(Y4, aes(x=seq(1:500), y=V1)) + geom_line() + ggtitle('Y4$V1')
# Plotting A3
A44 <- A4 %>% mutate(index = seq(1:500))
A44 <- melt(A44, id.vars ='index', variable.names = 'series')
ggplot(A44, aes(x=value)) + geom_histogram() + facet_wrap(~ variable)
# Checking the correlation among the explanatory variables
cor(A4)
# Linear models
print('Linear Model 4')
L4 = lm(as.matrix(Y4)~.,data = as.data.frame(A4))
summary(L4)
plot(L4)
# verifying the normality of the residuals
hist(L4$residuals, freq = FALSE)
# Shapiro Wilk test
shapiro.test(L4$residuals)
# Kolmogorov test for normality
ks.test(L4$residuals, "pnorm", mean = mean(L4$residuals),
        sd = sd(L4$residuals))
# Conclusion
# Based on R2 (and the adjusted version as well), Y4 is being
# modelled properly by A4. However, the residuals are not
# Gaussian and therefore the Fisher amd Student tests cannot be
# taken into consideration so we dont know exactly which
# coefficients can be used or disregarded. Furthermore, there is
# no correlation among the explanatory variables.

# Overall conclusion, based on the present tools, knowledge and
# skills, we cannot eliminate explanatory variables based on
# the T-student result IF there is correlation among them

###################################################
# Model selection homework
# ========================

# VARIABLE SELECTION PROCEDURE
# Method: Step by step forward with Global Fisher
# test as the criteria and the stopping criteria is based on
# identifying a lower F statistic. Once it is found, the
# algorithm will keep the previous best model

vsp <- function(data, outcome){
  data <- as.data.frame(data)
  outcome <- as.matrix(outcome)
  class(data)
  class(outcome)
  Ln <- lm(outcome, data)
  summary(Ln)
  print("Carajo")
  for (var in data){
    #Ln <- lm(as.matrix(outcome)~., data = as.data.frame(var))
    #summary(Ln
    print("Mierda")
  }
}

vsp(A1, Y1)
