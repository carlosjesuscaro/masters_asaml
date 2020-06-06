# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-06

# Initial setup
X = matrix(data=c(rep(1,500),
                  c(rep(1,110),rep(0,390)),
                  c(rep(0,110),rep(1,130),rep(0,260)),
                  c(rep(0,240),rep(1,140),rep(0,120)),
                  c(rep(0,380),rep(1,120))),ncol=5)

B = matrix(data=c(5,1.2,0.8,125,0.75),ncol=1)
Y = X%*%B + matrix(data=rnorm(500),ncol=1)

xf = factor(c(rep('A',110),rep('B',130),rep('C',140),
              rep('D',120)))

# Boxplot
boxplot(Y~xf)
# Model
mod = lm(Y ~ xf)

# Conclusions
# 2. Yes, the actor is influenial, greatly by C

# 3. Comparing the effect by pair


