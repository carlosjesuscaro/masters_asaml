library(lasso2)
data(Prostate)
Prostate$svi=as.factor(Prostate$svi)
Prostate$gleason=as.factor(Prostate$gleason)
dim(Prostate)
cor(Prostate[,-c(5,7)]) # Correlation can only be calculates
# with numerical values, not categorical. The outcome of the correlation
# is not enough to decide whether to supress a variable or not (unless it is
# 1 which means that they are identical). If there is a strong correlation,
# potentially while building the model we could do variable selection.
# Only at that moment but not with the correlation alone

# Splitting the dataset into training and test datasets
ind.test <- 4*(1:22)
Prostate.learn <- Prostate[-ind.test,]
Prostate.test <- Prostate[ind.test,]
ntest <- length(Prostate.test$lpsa)
nlearn <- length(Prostate.learn$lpsa)

# Defining a function to plot the residuals
plot.res <- function(x,y,title=" ")
{
	plot(x,y,col='blue',ylab='Residuals',xlab='fitted values',main=title)
	abline(h=0,col='green')
}

# building two new data sets without the qualitative variables
Prostate.learn1 <- Prostate.learn[,-c(5,7)]
Prostate.test1 <- Prostate.test[,-c(5,7)]
# Building a linear model based on the previous datasets
mod1 <- lm(lpsa~.,data=Prostate.learn1)
summary(mod1)
# Once the model is built:
# 1. The residuals section may give an indication whether the noise is Gaussian. This
# is not conclusive and further tests are needed but if the residuals are not simetrical
# at all, then it is a strong indicator that the noise is not Gaussian
# 2. The Coefficient estimations are calculated with the Least Squared criteria so they
# are valid regardless the nouise distribution
# 3. The R-squared results are also independent from the noise distribution. In this case,
# since it is multiple linear regression the Adjusted version is required as the Multiple
# R-squared increases in propoortion of the number explanatory variables
# 4. The rest of the output depends on the conclusion about the Gaussianity of the noise

# Plotting the Classicalresiduals
res <- residuals(mod1)
hist(res, freq = FALSE)
qqnorm(res)
# These residuals cannot be used as they are not iid. The variance is not the same

# Plotting the Standarized residuals
ress <- rstandard(mod1)
hist(ress, freq = FALSE)
qqnorm(ress)

# Plotting the Studiantized residuals (useful to detect outliers, bigger than absolute
# value of 2)
resu=rstudent(mod1)
hist(resu, freq = FALSE)
qqnorm(resu)
plot.res(predict(mod1),res)
plot.res(predict(mod1),ress)
plot.res(predict(mod1),resu)

# Another alternative is to use the Shapiro test:
shapiro.test(ress)
# p-value is 0.2705. Thus, it is gaussian

# By accepting the Gaussianity of the residuals, we can accept the Global Fisher test
# and the Student test per each coefficient. In this case, there are variable with
# large p-values which means thay they may be candidates for variable selection. There is
# only one varibale, lcp, with a p-value of 0.89692 which is extremly high and could
# be supressed at this point

# Computing the error from mod1
mean(res**2) # This is the learning error
pred.test1=predict(mod1,newdata=Prostate.test1)
res.test=pred.test1-Prostate.test1$lpsa
mean(res.test**2) # 0.5207949

# Variable selection
library(MASS)
# Backward method with Fisher criteria
modsel1=stepAIC(mod1,~.,direction=c("backward"),test="F")
mod0=lm(lpsa~1,data=Prostate.learn1)
modsel2=stepAIC(mod0,lpsa~lcavol+lweight+age+lbph+lcp+pgg45,data=Prostate.learn1,direction=c("forward"),test="F")
# Stepwise method with Fisher criteria
modsel3=stepAIC(mod1,~.,direction=c("both"),test="F")
# Stepwise method with AIC criteria
modsel4=stepAIC(mod1,~.,direction=c("both"))
# Stepwise method with BIC criteria
modsel5=stepAIC(mod1,~.,direction=c("both"),k=log(nlearn))
# In the case of variable selection, the p-value threshold used is 10%
# instead the usual 5%
# AIC  vs BIC: due to the difference in penalties, BIC will tend to return
# models that have fewer variables. This is why AIC is a more popular choice

# compute the test error for each model

# Comparing mod1, modsel4 and modsel5
# mod1 error: 0.5207949
# Calculating the error for modsel4
predsel4 <- predict(modsel4, newdata = Prostate.test1)
ressel4 <- predsel4 - Prostate.test1$lpsa
mean(ressel4**2) # 0.5115337
# Calculating the error for modsel5
predsel5 <- predict(modsel5, newdata = Prostate.test1)
ressel5 <- predsel5 - Prostate.test1$lpsa
mean(ressel5**2) # 0.4635027

# In practice, models with fewer variables are preferred as they can be computed faster

library(glmnet)
# Variable selection with Ridge method because alpha = 0
# Ridge does not perform variable selection
mod6=glmnet(Prostate.learn1[,-7],Prostate.learn1[,7],alpha=0)
plot(mod6)
abline(h = 0, col = "yellow")
# Based on the graph,  the value of the coefficients increases significantly as the L1
# norm increases

# Using cross validation to compare the models
mod6b=cv.glmnet(as.matrix(Prostate.learn1[,-7]),Prostate.learn1[,7],alpha=0)
mod6b
# The outp shows:
# min: it shows the lambda value for which the cross validation error is minimum
# 1se: it shows the lambda value for which the cross validation error is the minimum
# plus 1 standard error

# Using the lambda value from the previous step (1se)
mod7=glmnet(Prostate.learn1[,-7],Prostate.learn1[,7],alpha=0,lambda=mod6b$lambda.1se)
# Beta coefficients associated with the model
mod7$beta
# Coefficient for the interception
mod7$a0

# Variable selection with Lasso method because alpha = 1
mod8=glmnet(Prostate.learn1[,-7],Prostate.learn1[,7],alpha=1)
plot(mod8)
mod8b=cv.glmnet(as.matrix(Prostate.learn1[,-7]),Prostate.learn1[,7],alpha=1)
mod9=glmnet(Prostate.learn1[,-7],Prostate.learn1[,7],alpha=1,lambda=mod8b$lambda.1se)
# Beta coefficients associated with the model
mod9$beta
# Coefficient for the interception
mod9$a0
# Based on the variable selection made by Lasso, we can build the final model:
modf <- lm(lpsa~lcavol + lweight, data = Prostate.learn1)
summary(modf)
# The coefficients obtained from Lasso are biased estimators while the coefficients
# of the linear model are unbiased estimators
# Note: modf and modsel5 have the same results

#  ANOVA 1 factor
# svi is a categorical variable
model=lm(lpsa~svi,data=Prostate.learn)
summary(model)
res2=residuals(model)
hist(res2)
qqnorm(res2)
ress2=rstandard(model)
hist(ress2)
qqnorm(ress2)
resu2=rstudent(model)
plot.res(predict(model),res2)
plot.res(predict(model),resu2)
# Based on the graphs, we can accept the Gaussianity and homocedasticity of the noise
summary(model)
boxplot(lpsa~svi,data=Prostate.learn)

# ANOVA 2 factors
model2=lm(lpsa~svi*gleason,data=Prostate.learn)
res2b=residuals(model2)
ress2b=rstandard(model2)
hist(ress2b)
qqnorm(ress2b)
plot.res(predict(model2),res2b)
summary(model2)
anova(model2)
# The decision about which variable to use will be done with the Anova output and
# not with the Summary. In Anova, the first thing to look at is the cross effect because
# if it is acceptable, then there is an influence of the 2 factors. Otherwise, only one of
# the factors will be the influential. In this case, the cross effect has a veru high p-value
# so the cross effect is not influential. Thus, a new model considering only the additive
# effect is needed:
model3=lm(lpsa~svi+gleason,data=Prostate.learn)
anova(model3)

# Notes:
# The difference between model2 and model3 is that model2 considers the additive and
# cross effect of the categorical variables while model2 only considers the additive effect

# ANCOVA: linear model using quantitative and qualitative variables
model4=lm(lpsa~.,data=Prostate.learn)




