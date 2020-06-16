# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-15
library(rpart)

# Collecting the random samples from the Iris data set
u = sample(1:150,120)
# Building the learning data set from the U sample
learning = iris[u,]
# Building the testing data set from elements which are not in U
test=iris[-u,]

# Training the CART algorithm
Tree = rpart(learning[,5]~.,data=learning[,-5],cp=0.002,minsplit=2)
# Predicting with the CART algorithm
predict(Tree)
# using the training set, so the outcome isn't really a prediction
predict(Tree, type = 'class')
# now the outcome is a prediction
# this isnt needed in the regression setting

# checking whether we are in classification or regression
# multiply the point facot by the Y value, if true -> classification
# if false -> regression

TreeC = rpart(iris[,5]~.,data=iris[,-5])

TT1 = rpart(iris$Species~.,data=iris[,-5],control=
  rpart.control(cp=10^-9, minsplit = 2))

predict(TT1, newdta = c(Petal.length=3.2,Petal.Width=1.8,
Sepal.Length = 2.7, Sepal.Width = 6.3),type = 'class')

