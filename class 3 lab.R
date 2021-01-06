# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-05-30

library(dplyr)
library(ggplot2)
library(reshape)

# Uploading the data
Y1 <- read.table('Y1.txt')
Y2 <- read.table('Y2.txt')
Y3 <- read.table('Y3.txt')
Y4 <- read.table('Y4.txt')
A1 <- read.table('A1.txt')
A3 <- read.table('A3.txt')
A4 <- read.table('A4.txt')

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
L1 <- lm(as.matrix(Y1)~.,data = as.data.frame(A1))
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
L2 <- lm(as.matrix(Y2)~.,data = as.data.frame(A1))
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
L3 <- lm(as.matrix(Y3)~.,data = as.data.frame(A3))
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
# Plotting A4
A44 <- A4 %>% mutate(index = seq(1:500))
A44 <- melt(A44, id.vars ='index', variable.names = 'series')
ggplot(A44, aes(x=value)) + geom_histogram() + facet_wrap(~ variable)
# Checking the correlation among the explanatory variables
cor(A4)
# Linear models
print('Linear Model 4')
L4 <- lm(as.matrix(Y4)~.,data = as.data.frame(A4))
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
  flist <- vector()
  data_portion <- data.frame(matrix(NA, nrow = dim(data)[1], ncol = 1))
  for (i in 1:length(data)){
    if (i == 1)
      {
        data_portion[1] = data[1]
      }
    else
      {
        data_portion <- cbind(data_portion, data[i])
      }
    colnames(data_portion) <- factor(seq(1:(dim(data_portion)[2])))
    Ln <- lm(as.matrix(outcome)~., data = as.data.frame(data_portion))
    Lnf <- summary(Ln)
    flist <- cbind(flist, Lnf$fstatistic[1])
    if (i >= 2){
      if (flist[i-1] > flist[i]){
          result <- cbind(i -1, flist[i-1])
         return(result)
      }
    }
  }
}

final <- vsp(A4, Y4)
paste("The best result is achieved with the first ", final[1,1],
      "column(s) and with an F statistic value of ", final[1,2])

######################################################################
# Partial: based on p-value, choosing the best variable among all of them
vsp_fisher <- function(data, outcome){
  data <- as.data.frame(data)
  outcome <- as.matrix(outcome)
  fp_list <- vector()
  best_model <- data.frame(matrix(NA, nrow = dim(data)[1], ncol = 1))
  for (data_portion in data){
    Ln <- lm(as.matrix(outcome)~., data = as.data.frame(data_portion))
    Lnf <- summary(Ln)
    fp_list <- cbind(fp_list, pf(Lnf$fstatistic[1],Lnf$fstatistic[2],Lnf$fstatistic[3],
                                 lower.tail = FALSE))
  }
  best_model <- data[which.min(fp_list)]
  fp_list <- vector()
}

final <- vsp_fisher(A4, Y4)

######################################################################
# Using the existent libraries
library(MASS)
step.model <- stepAIC(L3, direction = "both", trace = FALSE)
summary(step.model)