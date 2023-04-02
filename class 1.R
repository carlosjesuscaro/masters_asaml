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
# a) even distribution of the error across the data set (symetry between the upper
# and the lower part)
# b) homoscedasticity can be appreciated (constant variance for the noise). If this
# condition is not satisfied, all the computation is useless
# * In this plot we are using the classical residuals
# 2. Normal Q-Q (quantile quantile plotting - y axis shows the observed quantiles):
# Q-Q means quantiles quantiles plotting. This is assuming that we have a Gaussian
# distribution. Essentially, the x-axis is the normal distribution quantile and y-axis
# is the quantile for each point of the observed data. Plotting them representes
# how close (or far) the quantiles from the observed data are to the qauntiles
# from the normal distribution. This is the normal QQ plot because we are using the
# normal distribution but another distribution could be used as well
# In this plot we are using the Standarized residuals (not the same as in
# residuals vs fitted plot)
# 3. Scale location. This plot helps to identifdy how the residuals are spread through
# the fitted values so that a potential concentration within a range of fitted values us more
# prominent than with other values
# 4. Residuals vs Leverage. It helps to identify outliers as well as influential points


# Personal example to ilustrate the values from F statistic
x1 = runif(100,-3,3)
x2 = runif(100,-3,3)
y1 = 3 - 2*x1 + 3*x2 + rnorm(100)
L1 = lm(y1~x1+x2)
summary(L1)




