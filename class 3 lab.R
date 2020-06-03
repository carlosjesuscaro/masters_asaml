# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-05-30

# Uploading the data
Y1 = read.table('Y1.txt')
Y2 = read.table('Y2.txt')
Y3 = read.table('Y3.txt')
Y4 = read.table('Y4.txt')
A1 = read.table('A1.txt')
A3 = read.table('A3.txt')
A4 = read.table('A4.txt')

# Linear models
L1 = lm(as.matrix(Y1)~.,data = as.data.frame(A1))
summary(L1)
# we dont have the correct explanatory variables
# The first thing to look should be the Multiple R-squared and
# Adjusted R-squared (which are very bad in this case)

L2 = lm(as.matrix(Y1)~.,data = as.data.frame(A1))
summary(L2)
# similar as the previous case, the data isn't a good fit for the
# model

L3 = lm(as.matrix(Y3)~.,data = as.data.frame(A1))
summary(L3)
# Much better outcome as the Global Fisher testing and R^2 squared
# give better results and the individual tests are fine (all of them)




