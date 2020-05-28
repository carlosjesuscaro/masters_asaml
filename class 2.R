# Defining the equation
x = runif(50,-7,7)
y = 5-6*x+rnorm(50)
# Ploting the equation
plot(x,y)
title('Linear equation')
grid()
# Linear model
L = lm(y~x)
L
# the output is the coefficienties: (Intercept -> a0hat)/ (x -> a1hat

# L$coefficients: exact values
L$coefficients
abline(L,col='red')

# Calling for help in R
#help(predict.lm)

# Prediction of y_hat associated to the training sample
# Prediction associated to a new observaton
predict(L)
# Creating new data set
new = data.frame(x = seq(-3,3,0.1))

# Obtaining the confidence interval and prediction
pred.w.plim <- predict(L, new, interval = "prediction")
pred.w.clim <- predict(L, new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]), lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
