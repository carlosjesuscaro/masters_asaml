# Title     : PCA Example
# Objective : To understand and interpret the PCA algorithm
# Created by: carloscaro
# Created on: 2020-06-20

# The following matrix represents the grades from 9 students in 4 classes
A = matrix(c(6,8,6,14.5,14,11,5.5,13,9,6,8,7,14.5,14,10,7,12.5,
             9.5,5,8,11,15.5,12,5.5,14,8.5,12.5,5.5,8,9.5,15,12.5,
             7,11.5,9.5,12),ncol = 4)
colnames(A) = c('Math','Physics','French','English')

pcA = princomp(A)
plot(pcA)
biplot(pcA)
pcA$loadings
pcA$scores
