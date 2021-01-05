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
# Linear models
print('Linear Model 1')
L1 = lm(as.matrix(Y1)~.,data = as.data.frame(A1))
summary(L1)
plot(L1)
# verifying the normality of the residuals
hist(L1$residuals, freq = FALSE)
# Shapiro Wilk test
shapiro.test(L1$residuals)
# Conclusion
# This is not a multi linear model as the adjusted R2 is very low
# plus the residuals are not in the Gaussian distribution

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
# Conclusion
# This is a much better case where we can say that Y2 can be modeled
# through a multi-linear regression model. The residuals are Gaussian
# so we can accept the Fisher and Student test
# The R2 (and Adjusted R2) show a good performance where the
# most critical coefficients are V1 and V5







print('Linear Model 3')
L3 = lm(as.matrix(Y3)~.,data = as.data.frame(A3))
summary(L3)

print('Linear Model 4')
L4 = lm(as.matrix(Y4)~.,data = as.data.frame(A4))
summary(L4)



