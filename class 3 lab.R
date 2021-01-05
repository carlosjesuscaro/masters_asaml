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

print('Linear Model 2')
L2 = lm(as.matrix(Y2)~.,data = as.data.frame(A1))
summary(L2)

print('Linear Model 3')
L3 = lm(as.matrix(Y3)~.,data = as.data.frame(A3))
summary(L3)

print('Linear Model 4')
L4 = lm(as.matrix(Y4)~.,data = as.data.frame(A4))
summary(L4)



