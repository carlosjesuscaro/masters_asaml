# Title     : Class 8
# Objective : Overall review
# Created by: carloscaro
# Created on: 2020-06-19

library(ggplot2)

# Loading the dataset. ATTENTION to the parameters used with read.table
ds_ozone <- read.table('Dataset_ozone.txt', header = TRUE, sep = ';',dec = ',')
attach(ds_ozone)
summary(ds_ozone[c("maxO3","T12")])

# Goal: to explain max03 through T12 - SIMPLE LINEAR MODEL
# ========================================================

# Using ggplot to explore the data
ggplot(ds_ozone,aes(x=T12,y=maxO3))+
  geom_point()+
  stat_smooth(method = 'lm', se=FALSE)+
  xlab('T12')+
  ylab('maxo3')

 # Based on the graph, it seems linear so we are going to proceed with simple linear regression
reg_simpl = lm(maxO3~T12,data=ds_ozone)

# Comparing the observed values vs the fitted values
maxo3_adjusted = reg_simpl$fitted.values #yi_hat
ggplot(ds_ozone,aes(x=maxO3,y=maxo3_adjusted))+
  geom_point()+
  geom_abline(intercept = 0,slope = 1,color='red')+
  xlab('maxO3')+
  ylab('maxO3_adjusted')

# MULTIPLE LINEAR MODEL (only w/ quantitative parameters)
# =======================================================
# Quantitative param: T9, T12, T15, Ne9, Ne12, Ne15, Vx9, Vx12, Vx15, maxO3v
# Qualitative param: vent, pluie
reg_multi = lm(maxO3~T9+T12+T15+Ne9+Ne12+Vx9+Vx12+Vx15+maxO3v,data = ds_ozone)
summary(reg_multi)
plot(reg_multi)
# Gaussian assumption could be tested with the Shapiro test
# it is Gaussian if we accept the null assumption (H0)

# Fisher (global test) is about:
# H0 = all coefficientes are 0 / H1 = coefficientes are not 0

# Checking the correlation of the quantitative parameters from the data
cor(ds_ozone[,3:12])

# calculating PCA (Principal component analisis)
# this can only be performed w/ quantitative variables
ozonepca = ds_ozone[,3:12]
# the PCA function is 'princomp' which should be told whether we want to normalize the
# data or not
pcaozone = princomp(ozonepca) # non normalized PCA
pcaozone$center # mean of the variables
pcaozone$scale  # notion of scaling (with which quantity we do the normalization
# it is only 1 in the case of non normalized
pcaozone$loadings # it shows the list of the new variables
pcaozone$scores # projection of each individual wiith the new variables
s = pcaozone$sdev
cumsum(s^2/sum(s^2))
# The outcome is the amount of information that we can retain by the
# accumulation of the components. For example:
# Comp. 1: 0.9238498 => This means that 92% of the information is kept in Comp. 1
# Comp. 2: 0.9657107 => This means that 96% of the information is kept in Comp 1 and 2
# In practice, usually we keep only the first 2 components (q = 2)

# Manual calculation
V = cov(ozonepca)
ev = eigen(V)
eigenvalues = ev$values
sqrt(eigenvalues)
# The difference between the manual calculation and the princomp outcome is because
# the empirical variance has 2 options: 1/n or 1/(n-1). Thus, the covariance is
# going to follow the same form

# This difference doesnt happen with normalized PCA because there is only one
# formula for the correlation
pcanozone = princomp(ozonepca, cor=TRUE)  # normalized PCA
pcanozone$scale # here are the values used to normalize

# Bulding the new data set from pcaozone (non normalized PCA)
newdataset = pcaozone$scores[,1:2]
# Bulding a new model based on the new dataset with q=2
newdata = cbind(maxO3, newdataset)
colnames(newdata) = c('maxO3','comp1','comp2')
reg2= lm(maxO3~comp1+comp2,data=as.data.frame(newdata))
# Although now we have been able to run a new model with only 2 variables
# this sprocess is not actually variable selection

# Variable selection can be done through Lasso, Ridge or Fisher test
