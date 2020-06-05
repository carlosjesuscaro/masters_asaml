# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-05

# Defining the variables
x = matrix(rnorm(100 * 20), 100, 20)
y = rnorm(100)
# Ordinary linear model (for comparison purposes)
L = lm(y ~ x)
summary(L)

