# Title     : Regression Tree
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-12

# Loading the library associated with CART
library(rpart)
# Classification example
data(iris)
Tree1 = rpart(iris$Species~.,data=iris[,-5])
plot(Tree1)
plotcp(Tree1)
# This is a more proper tree
Tree2 = rpart(iris$Species~.,data=iris[,-5],
              control = rpart.control(cp=10^-9,
              minsplit = 1))
plot(Tree2)
plotcp(Tree2)
# cp: complex parameter
# minsplit: minimum number of observations that must exist in
# a node in order for a split to be attempted

# Regression example
data(mtcars)
Tree3 = rpart(mtcars$mpg~.,data=mtcars[,-1])
plot(Tree3)
plotcp(Tree3)
Tree4 = rpart(mtcars$mpg~.,data=mtcars[,-1],
              control = rpart.control(cp=10^-9,
                                      minsplit = 2))
plot(Tree4)
plotcp(Tree4)
printcp(Tree4)
summary(Tree4 )

