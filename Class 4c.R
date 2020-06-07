# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-07

X = matrix(data=c(rep(1,10),
                  c(rep(1,3),rep(0,7)),
                  c(rep(0,3),rep(1,3),rep(0,4)),
                  c(rep(0,6),rep(1,4)),
                  c(1,1,0,1,0,0,1,1,0,0),
                  c(0,0,1,0,1,1,0,0,1,1),
                  c(1,1,0,0,0,0,0,0,0,0),
                  c(0,0,1,0,0,0,0,0,0,0),
                  c(0,0,0,1,0,0,0,0,0,0),
                  c(0,0,0,0,1,1,0,0,0,0),
                  c(0,0,0,0,0,0,1,1,0,0),
                  c(0,0,0,0,0,0,0,0,1,1)),
                  ncol = 12)

B = matrix(data=c(1,2,3,4,2,2,1,2,1,3,1,2),ncol = 1)
Y = X%*%B + rnorm(10)
F1 = factor(c(rep(1,3),rep(2,3),rep(3,4)))
F2 = factor(c(1,1,2,1,2,2,1,1,2,2))

mod1 = lm(Y ~ F1 + F2) # This doesn include the cross effect
# R knows this is an ANOVA because F1 and F2 are factors
mod2 = lm(Y ~ F1 + F2 + F1*F2)

anova(mod1)
anova(mod2)
# Based on the results, we can determine whether we can supress the cross effect or not.
# If yes, then we can keep only the model with (F1 + F2)

FF1 = as.numeric(F1)
FF2 = as.numeric(F2)

mod11 = lm(Y~FF1+FF2)
# R is going to perform as classical linear regression becaise FF1 and FF2
# numbers and not factors




