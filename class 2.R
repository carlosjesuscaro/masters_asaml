# Defining the equation
x = runif(50,-7,7)
y = 5-6*x+rnorm(50,0,10)
# Ploting the equation
plot(x,y)
title('Linear equation')
grid()
# Linear model
L = lm(y~x)
summary(L)
# the output is the coefficienties: (Intercept -> a0hat)/ (x -> a1hat

# L$coefficients: exact values
L$coefficients
abline(L,col='black')

# Calling for help in R
#help(predict.lm)

# Prediction of y_hat associated to the training sample
# Prediction associated to a new observaton
predict(L)
# Creating new data set
new = data.frame(x = seq(-7,7,0.1))

# Obtaining the confidence interval and prediction
# The prediction always require a new data set (new observation)
pred.w.plim <- predict(L, new, interval = "prediction")
# The confidence does not always require a new data set E[y]
pred.w.clim <- predict(L, new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]), lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
title('Prediction and confidence interval')
grid()
# Plotting individually (just replace plim or clim)
#matplot(new$x, pred.w.plim, lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
#grid()

# Plotting all together
# Training data set
plot(x,y)
title('Prediction and confidence interval PLUS training set')
matlines(new$x, cbind(pred.w.clim, pred.w.plim[,-1]), lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
points(L$model$y,col='yellow')
grid()

