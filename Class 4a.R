# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-05

X = matrix(data=c(rep(1,100),
                  c(rep(1,20),rep(0,80)),
                  c(rep(0,20),rep(1,25),rep(0,55)),
                  c(rep(0,45),rep(1,20),rep(0,35)),
                  c(rep(0,65),rep(1,10),rep(0,25)),
                  c(rep(0,75),rep(1,25))),ncol=6)

B = matrix(data=c(4,0.5,0.4,0.6,0.2,0.1),ncol=1)
Y = X%*%B + matrix(data=rnorm(100),ncol=1)
xf = factor(rep('A',20,rep('B',25),rep('C',20),
                rep('D',10),rep('E'),25))

