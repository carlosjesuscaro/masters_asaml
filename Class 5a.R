# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-08

X = data.frame(T1=c(5,8,7,7,10,8), T2=c(4,6,6,3,5,6),
               T3=c(6,4,4,5,4,3),T4=c(7,4,6,6,3,5),
               T5=c(9,3,5,7,7,6))

delai <- stack(X)$values # This is the response
# Transpose of Y, the big vector

treatment <- factor(rep(c('T1','T2','T3','T4','T5'),each=6))
# ^ that's the factor
# shortcut to write Tn:
# paste('T',1:5,sep='') .... useful if we have to write many numbers
plot(delai~treatment,col='green')

# Because we have a very small data set, we are accepting H0 and
# not doing the test for variance and Gaussian
myaov = aov(delai~treatment)
summary(myaov)
# based on the p-value=1%, we take H0 and we conclude that there isnt
# much influence

model <- lm(delai~treatment)
summary(model)

# Comparing all the factors
library(gmodels)
cmat <- rbind(":2 versus 3"=c(0,1,-1,0,0))
fit.contrast(myaov,treatment,cmat)
# the output shows a big p-value so we accept H0
# this means that T2 and T3 are considered equals

# In theory, we should perform this test for every combination
# but it would take a lot of time.
# Here is how to make it collectively:
pairwise.t.test(delai,treatment,p.adjust="bonf")
# the output shows "t tests wit pooled SD" which means that for
# level, the variances are equal
# For example, we can conclude that T3 and T1 are different
# because their p-value is 1.4% so we accepted H1

# Checking if 0 is inside
TukeyHSD(myaov)
par(las=1)
plot(TukeyHSD(myaov))
# This shows the difference among all pairs and we see that
# between T1 and T3, the difference doesnt include 0 which
# explains why they are different (we can also see why other facors
#  are considered the same)
# we can change the confidence level by using the parameter:
# conf.level