# Defining the the base equations
x = runif(100,-3,3)
y = 3 - 2*x + rnorm(100)

# Plotting
plot(x,y)
title(main='Linear Regression Example',xlab = 'Uniform points',ylab = 'Linear EQ w/ noise')

# Linear regression
L = lm(y~x)

#Summary
summary(L)
names(L)
L$coefficients
L$rank
L$call
L$model

# Calculating (sigma_hat)^2
sigmahatn2 = sum(L$residuals^2)/98
sigmahatn2
# Manually calculating the residual standard error, equivalent to the square root of
# sigma_hat square
sqrt(sigmahatn2)

# Checking that the noise is Gaussian (through the residuals)
R = L$residuals
hist(R,freq = FALSE) # it has the basic shape of the Gaussian distribution
plot(L) # L is the linear model so plotting it gives us the following plots:
# 1. Residual vs fitted:
# even distribution of the error across the data set (symetry between the upper
# and the lower part)
# homoscedasticity can be appreciated (constant variance for the noise). If this
# condition is not satisfied, all the computation is useless
# 2. Normal Q-Q:
# Q-Q means quantiles quantiles plotting. This is assuming that we have a Gaussian
# distribution
# 3. Scale location
# 4. Residuals vs Leverage

# one of the graphs shows with a red line the expectation of the noise (zero) and the points around the line.
# If the number of points on each side of the red line is equal, then we can accept the solution (concept of 
# homoscedasticity). It is important to check that we have the same variance across the dataset




