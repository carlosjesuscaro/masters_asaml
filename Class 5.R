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
# we need to verify the assumptions of the noise
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
# Because we are working on the whole dataset, we are working
# with the residuals

print('Pre-packaged test')
library(car)
leveneTest(maxO3,vent,center=median)
leveneTest(model$residuals,vent,center=median)

# Barlett test
bartlett.test(model$residuals~vent)
# The p-value is 0.8. Thus, we accept H0 and we
# accept that all the levels have the same variance

# For Gaussianity:
# ================
# Shapiro test (preferred over other tests)
select.est <- ozone['vent']=='Est'
# we can perform the same for every label
shapiro.test(ozone[select.est,"maxO3"])
# the output shows a high p-value. Thus, we accept
# H0 => data is Gaussian (H1 => not Gaussian)

# remark: here we are on one label which is why
# we can work directly on the response variable and
# not on the residuals (which would be the case if
# we are working with the whole data set because the
# response for all individuals are not identically
# distributed - not the same expectation,
# same variance though)

qqnorm((ozone[select.est,"maxO3"]))

# If the graph seems to be across the line, we accept
# the Gaussian

select.nord <- ozone['vent']=='Nord'
shapiro.test(ozone[select.nord,"maxO3"])
qqnorm((ozone[select.nord,"maxO3"]))

select.sud <- ozone['vent']=='Sud'
shapiro.test(ozone[select.sud,"maxO3"])
qqnorm((ozone[select.sud,"maxO3"]))

select.ouest <- ozone['vent']=='Ouest'
shapiro.test(ozone[select.ouest,"maxO3"])
qqnorm((ozone[select.ouest,"maxO3"]))

# ANOVA testing
ozone.aov <- aov(maxO3~vent)
summary(ozone.aov)
# Conclusion: the wind does have an influence in the Ozone layer
# aov gives us this conclusion but does not give us the value

res.ozone <- rstudent(ozone.aov)
plot(res.ozone~vent)
# There are 112 observation so 5% is around 6 and we have around 7
# observations that are out of the region (95& ~ 2 threshold)

summary(lm(maxO3~vent,data=ozone))
# Here the overall goal is to confirm that there is an influence
# not to find the right model
# In this case, the first label is the reference cell ('est')

summary(lm(maxO3~C(vent,base=2),data=ozone))
# We have changed the reference cell to the second label

summary(lm(maxO3~C(vent,sum),data=ozone))
# Imposing another constraint:
# mu_hat =mh
# mh4 = - (mh1 + mh2 + mh3)