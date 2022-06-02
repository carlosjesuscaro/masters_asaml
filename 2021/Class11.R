library(VSURF)
data("toys")
head(toys)

# Trying with Linear regression
L <- lm(toys$y~., data = toys$x)
summary(L)
# Although it is possible to get an output, the linear regression
# model is meant to work only with a numeric response variable.
# Furthermore, there is a lot of NA because the rank of X is 100
# When p > n, there will be linear dependency among the columns

# Random Forest approach
set.seed(3101318)
toys.vsurf <- VSURF(x = toys$x, y = toys$y, mtry = 200)
# mtry: number of decision trees
names(toys.vsurf)
# It brings a lot of information including the variables selected
# after each step
plot(toys.vsurf)

# Using the same data but with Decision Tree (CART algorithm)
library(rpart)
learn <- toys$x[,c(3,2,6,5)]
head(learn)
T <- rpart(toys$y~., data = learn, control = rpart.control(cp=0, minsplit = 2))
# Where T is the maximal tree

# Pruning:
printcp(T)
plotcp(T)
