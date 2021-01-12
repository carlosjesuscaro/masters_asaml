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
# library
library(multcompView)
TUK <- TukeyHSD(x=aov(mod), 'xf', conf.level=0.95)
plot(TUK , las=1 , col="brown")



# Example from the internet, not related to the class homework
# Create data
set.seed(1)
treatment <- rep(c("A", "B", "C", "D"), each=20)
value=c( sample(2:5, 20 , replace=T) ,
         sample(6:10, 20 , replace=T),
         sample(1:7, 20 , replace=T),
         sample(3:10, 20 , replace=T))
data=data.frame(treatment,value)

# What is the effect of the treatment on the value ?
model=lm( data$value ~ data$treatment )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")

