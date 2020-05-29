print('Exercise 1')

# Defining the training data set
x = runif(100,-7,7)
y = 3 - 2*x + rnorm(100,500,150)
# y = 3 -2*x + rpois(100, lambda = 1)
# Plotting the training data set
plot(x,y)
title('Training data set')
grid()

# Linear model
L = lm(y~x)
summary(L)
abline(L, col='red')
points(x,L$fitted.values,pch='X',col='blue')

# Manual calculation of sigma_hat
sigma_hat = sqrt(sum((y-L$fitted.values)^2)/L$df.residual)

# Residuals = y - fitted values
# Histogram - residuals
hist(L$residuals,freq = FALSE)

# Plotting the complete outcome of the linear model L
plot(L)
# There are 4 plots as a result:
# 1. Residuals vs Fitted
# Homoscedasticity. This assumption means that the
# variance around the regression line is the same for
# all values of the predictor variable (X). In other words,
# same variance across all the data set
# 2. QQ plot - quantile/quantile plotting
# Assuming a Gaussian distribution
# Standarized residuals ~ theoretical quantiles
# If the points are tight to the line, we can accept the
# Gaussian noise
# 3. Scale location
# Standarized residuals ~ observed quantiles
# 4. Residuals vs Leverage
# It helps you identify influential data points on your model

# Prediction
pr = predict(L)
plot(pr,col='green')
title('Only prediction')
grid()



