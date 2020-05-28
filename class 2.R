# Defining the equation
x = runif(50,-7,7)
y = 5 - 6*x + rnorm(50,0,45)
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
abline(L,col='red')

# Help in R
#help(predict.lm)

# Prediction of y_hat associated to the training sample
# Prediction associated to a new observaton
#predict(L)
# Inserting the prediction points into the graph (blue X)
points(x,predict(L),pch='X',col='blue')

# Creating new data set
new = data.frame(x = seq(-7,7,length.out = 50))
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
grid()
title('Prediction and confidence interval PLUS training set')
# Confidence levels in prediction and E[y]
matlines(new$x, cbind(pred.w.clim, pred.w.plim[,-1]), lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
# Plotting the linear model with the training set
points(x,predict(L),pch='X',col='blue')
# Plotting the prediction with a new data set
points(x,predict(L,new),pch='P',col='purple')
print('Summary of prediction with testing data')
summary(predict(L,new))

