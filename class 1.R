# Defining the the base equations
x = runif(100,-3,3)
y = 3 - 2*x + rnorm(100)

# Plotting
plot(x,y)
title(main='Linear Regression Example',xlab = 'Uniform points',ylab = 'Linear EQ w/ noise')

# Linear regression
L = lm(x~y)
sum(L)