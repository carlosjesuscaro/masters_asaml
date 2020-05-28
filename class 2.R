# Defining the equation
x = runif(50,-7,7)
y = 5-6*x+rnorm(50)
# Ploting the equation
plot(x,y,)
title('Linear equation')
grid()
# Linear model
L = lm(y~x)
L
# the output is the coefficienties:
# (Intercept -> a0hat)
# (x -> a1hat

# L$coefficients: exact values
L$coefficients
abline(L,col='red')

help(predict.lm())
