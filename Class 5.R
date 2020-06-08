# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-07

# Loading the data
ozone <- read.table('ozone.txt')
head(ozone)
dim(ozone)
# we have 112 samples and 13 variables

#  How to see if there is an influence of the wind in
# the ozone concentration which is max03
# ozone$vent
# or
attach(ozone)
# now we can call every variable from ozone directly

summary(ozone[c("maxO3","vent")])

# boxplot of the variable max03 with respect to the
# labeled of wind
plot(maxO3~vent,data=ozone,pch=15,cex = 0.5,col = 'green')

# ANOVA with 1 Factor and 4 labels
# wind: east, north, south and west
model = aov(maxO3~vent)

#  before analyzing the outputs of tests
# we need tlo verify the assumptions of the noise
# - independency: by looking the experiments
# - gaussianity: normal qq-plot
#                 kolmogorov test
#                 shapiro test
# - homoscedasticty (same variance):
#                   - bartlett test
#                   - levene test

# Equality of the variances
# =========================
var1 = var(maxO3[vent=='Est'])
var2 = var(maxO3[vent=='Nord'])
var3 = var(maxO3[vent=='Ouest'])
var4 = var(maxO3[vent=='Sud'])
# we need to compare:
# var1 with var2,3,4
# var2 with var3,4
# var3 with var4

# Levene test
# this test will perform a fisher test and if we
# H0, then we can assume all variances are equals
summary(aov(abs(model$residuals)~vent))
print('Pre-packaged test')
library(car)
leveneTest(maxO3,vent,center=median)
leveneTest(model$residuals,vent,center=median)

# Barlett test
bartlett.test(model$residuals~vent)
# The p-value is 0.8. Thus, we accept H0 and we
# assume that all the levels have the same variance

# For Gaussianity:
