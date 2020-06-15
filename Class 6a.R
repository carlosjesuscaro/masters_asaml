# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-15

# Collecting the random samples from the Iris data set
u = sample(1:150,120)
# Building the learning data set from the U sample
learning = iris[u,]
# Building the testing data set from elements which are not in U
test=iris[-u,]

