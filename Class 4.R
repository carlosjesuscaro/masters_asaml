# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-05

library(glmnet)
# glm: generalized linear model
# Defining the variables
#x = matrix(rnorm(100 * 20), 100, 20)
x = matrix(rnorm(100 * 10), ncol = 10)
y = rnorm(100)
# Ordinary linear model (for comparison purposes)
L = lm(y ~ x)
summary(L)
# Using glmnet library (from the example)
fit1 = glmnet(x, y, alpha = 0)
fit2 = glmnet(x, y, alpha = 1)
# alpha = 0 ==> Ridge
# alpha = 1==> Lasso
plot(fit1)
plot(fit2)

fit2cv = cv.glmnet(x,y)
names(fit2)
names(fit2cv)
# lambda.min => best value of lambda such as the cross
# validation error is the smallest one
# lambda.1se => in general, bigger than lambda.min

# Forcing the lambda
fit22 = glmnet(x,y,lambda = fit2cv$lambda.1se)
print("Fit2 Model: ")
print(coefficients(fit2))
print("Fit2 Model with forced Lambda: ")
print(coefficients(fit22))
print("Fit2CV: ")

#****************
# Another example
B = matrix(data=0,ncol=5,nrow=100)
B[,1] = runif(100,-4,3)
B[,2] = rexp(100,4)
B[,3] = rexp(100,0.02)
B[,4] = rpois(100,10)
B[,5] = rf(100,2,5)
A = -2 + 3*B[,2] - 5*B[,1] + B[,4] + rnorm(100)
ABfitcv = cv.glmnet(B,A)
plot(ABfitcv)
ABfit = glmnet(B,A,alpha = 1,lambda = ABfitcv$lambda.1se)
plot(ABfit)
# Forcing the lambda isnt a good practice because the
# outcome is a BIAS estimator.
# The right way is to use lasso to ave the suppoort of
# variables that need to be supressed and THEN we use
# a ordinary least squared (OLS) which is an UNBIASED estimator

BSEL <- B[,1:4]
ABOLS <- lm(A~., data = as.data.frame(BSEL))
summary(ABOLS)
# In the case of VERY large datasets, normally Map Reduced is used but this
# requires to split the data. By doing this, LASSO amd Ridge should not
# be used because the 'addition' of the results from these algorithms
# will not be the same as if these algorithms were applied to the
# complete dataset. On the other hand, OLS can be used with Map Reduced
# and the results can be 'added' without affecting the final result
# as it will be the same as if it was calculated with the whole
# dataset without splitting it
