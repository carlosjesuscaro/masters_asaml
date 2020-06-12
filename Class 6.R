# Title     : Regression Tree
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-12

# Loading the library associated with CART
library(rpart)
data(iris)
Tree1 = rpart(iris$Species~.,data=iris[,-5])
# This is a more proper tree
Tree2 = rpart(iris$Species~.,data=iris[,-5],
              control = rpart.control(cp=10^-9,
              minsplit = 1))
