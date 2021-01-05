# Defining the equation
x = runif(50,-7,7)
y = 5 - 6*x + rnorm(50, 0,50)
# Ploting the equation
plot(x,y)
title('Linear equation')
grid()
# Linear model
L = lm(y~x)
summary(L)
# the output is the coefficienties: (Intercept -> a0hat)/ (x -> a1hat)

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
# Plotting the linear model with the training set (E[y])
points(x,predict(L),pch='X',col='blue')
# Plotting the prediction with a new data set (prediction)
points(x,predict(L,new),pch='@',col='purple')
print('Summary of prediction with testing data')
summary(predict(L,new))

print('Exercise 2 - Multivariable linear problem')
A = matrix(0,nrow = 50,ncol = 50)
A[,1] = rexp(50,0.4)
A[,2] = rnorm(50,3,0.5)
A[,3] = rpois(50,0.8)
A[,4] = runif(50,-5,3)

y = 3 + 2*A[,1] - 5*A[,2] + 7*A[4] + rnorm(50)
L1 = lm(y~.,data=as.data.frame(A))
L1
summary(L1)
plot(L1)
# = cbind(rep(1,50),A)

# rank = 5
# From the output: F statistic: 4773 on 4 (rank -1) and (n - rank)

