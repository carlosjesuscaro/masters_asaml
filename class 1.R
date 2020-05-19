# Defining the the base equations
x = runif(100,-3,3)
y = 3 - 2*x + rnorm(100)

# Plotting
plot(x,y)
title(main='Linear Regression Example',xlab = 'Uniform points',ylab = 'Linear EQ w/ noise')

# Linear regression
L = lm(y~x)
summary(L)
names(L)
L$coefficients
L$rank
L$call
L$model
sigmahatn2 = sum(L$residuals^2)/98
sigmahatn2
sqrt(sigmahatn2)
summary(L)

# Checking that the noise is Gaussian (through the residuals)
R = L$residuals
hist(R,freq = FALSE) 
plot(L) 
# one of the graphs shows with a red line the expectation of the noise (zero) and the points around the line.
# If the number of points on each side of the red line is equal, then we can accept the solution (concept of 
# homoscedasticity). It is important to check that we have the same variance across the dataset


 