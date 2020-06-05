# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-05

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


