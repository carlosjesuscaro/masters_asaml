# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-05-30

A = matrix(0,ncol=3,nrow=50)
A[,1] = rexp(50,0.2)
A[,2] = rexp(50,7)
A[,3] = runif(50,-5,6)
Y = 5 - 3*A[,1] + 2*A[,2] - A[,3] + rnorm(50)

L = lm(Y~.,data = as.data.frame(A))
summary(L)

Ab = runif(50,2,8)
A = cbind(A,Ab)
Y = 5 - 3*A[,1] + 2*A[,2] - A[,3] + rnorm(50)
dim(A)

L = lm(Y~.,data = as.data.frame(A))
summary(L)