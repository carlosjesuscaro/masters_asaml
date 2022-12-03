# Author: Carlos Jesus Caro
# Email: carlos.jesus-caro@edu.dsti.institute

##############################################################################
# Exercise 1
##############################################################################

# Importing the data
data <- read.table('./FinalExam/data1.txt', header = FALSE)
head(data)
class(data$V1)

# Option 1
##########

# Plotting the histogram
hist(data$V1, freq = FALSE)
# Note: Based on the histogream, it looks like an Uniform distribution

# Applying the Kolmogorov-Smirnov test
ks.test(data$V1, punif, min(data$V1), max(data$V1))
# Note: p-value: 0.783 ==> H0 is not rejected, data$V1 seems to be a uniform
# random variable

# Option 2
##########

# Calculating the time arrival between 2 points
data_diff <- diff(data$V1)

# Histogram of the time difference between 2 points from data$V1
hist(data_diff, freq = FALSE)
# Note: Based on the histogream, it looks like an Exponential distribution

# Performing the Kolmogorov - Smirnov test
ks.test(data_diff, pexp, 1 / mean(data_diff))
# Note: p-value: 0.5568 ==> We accept H0, the difference between 2 points of data$V1
# is an exponential random variable

# Calculating lambda
lambda_hat <- 1 / mean(data_diff)
lambda_hat

##############################################################################
# Exercise 2
##############################################################################

# Importing the data
uk_train <- read.table('./FinalExam/ukcomp1_r.dat', header = TRUE)
uk_test <- read.table('./FinalExam/ukcomp2_r.dat', header = TRUE)

# Exploring the training data set
head(uk_train)
class(uk_train)
dim(uk_train)

# Exploring the test data set
head(uk_test)
class(uk_test)
dim(uk_test)

# Note: All variables are numeric

# Checking the correlation
cor(uk_train)
# Note: There is no indication that a variable can be supressed from
# the correlatin analysis

# Building a linear model
lm_model <- lm(RETCAP~., data = uk_train)
summary(lm_model)
#  Note: Wihtout validating the noise, we can only consider the value
# coefficients since they are calculated with the least squared method
# and the adjusted R^2 value. Additional interpretations require the
# validation of the noise

# Validating the noise
# Step 1: Studiantized residuals
st_res <- rstudent(lm_model)
# Step 2: Histogram analysis
hist(st_res, freq = FALSE)
# Note: it seems to be Gaussian but it is not conclusive
# Step 3: QQ plot
qqnorm(st_res)
# Note: it seems to be ok for a Gaussian noise
# Step 4: checking homoscedasticity
plot.res <- function(x,y,title=" ")
{
  plot(x,y,col='blue',ylab='Residuals',xlab='fitted values',main=title)
  abline(h=0,col='green')
}
plot.res(predict(lm_model), st_res)
# Note: it looks ok for homscedasticity
# Applying the Shapiro test
shapiro.test(st_res)
# Notes: p-value = 0.167 ==> The residuals are Gaussian

# Variable selection
library(MASS)
# 1. AIC Backwards
mod_sel_back <- stepAIC(lm_model, ~., direction = c("backward"), test = "F")
# Note: AIC = -202.44
# Calculating the error
pred_aic_back <- predict(mod_sel_back, newdata = uk_test)
res_aic_back <- pred_aic_back - uk_test$RETCAP
error_aic_back <- mean(res_aic_back^2)
error_aic_back

# 2. AIC Forwards
mod_forward <- lm(RETCAP~1,data = uk_train)
mod_sel_forw <- stepAIC(mod_forward, RETCAP~WCFTCL+WCFTDT+GEARRAT+LOGSALE+
  LOGASST+NFATAST+CAPINT+FATTOT+INVTAST+PAYOUT+QUIKRAT+CURRAT,
                        data = uk_train,direction=c("forward"),test="F")
# Note: AIC = -201.57
mod_sel_forw <- stepAIC(lm_model, ~., direction = c("forward"), test = "F")# Calculating the error
pred_aic_forw <- predict(mod_sel_forw, newdata = uk_test)
res_aic_forw <- pred_aic_forw - uk_test$RETCAP
error_aic_forw <- mean(res_aic_forw^2)
error_aic_forw

# 3. AIC Stepwise
mod_sel_step <- stepAIC(lm_model, ~., direction = c("both"), test = "F")
# Note: AIC = -202.44
# Calculating the error
pred_aic_step <- predict(mod_sel_step, newdata = uk_test)
res_aic_step <- pred_aic_step - uk_test$RETCAP
error_aic_step <- mean(res_aic_step^2)
error_aic_step

# 4. Lasso
library(glmnet)
# Finding the best lambda with cross validation
lasso_cv <- cv.glmnet(as.matrix(uk_train[, -1]), uk_train[, 1], alpha=1)
# Building the model with Lasso and the best lambda
var_sel_lasso <- glmnet(uk_train[, -1], uk_train[, 1], alpha = 1,
                        lambda = lasso_cv$lambda.1se)
# Observing the selected variables by Lasso
var_sel_lasso$beta
# Note: Selected variables = CAPINT, LOGSALE, CURRAT, NFATAST, FATTOT,
# PAYOUT, WCFTCL

# Building the model with Lasso selected variables
mod_sel_lasso <- lm(RETCAP ~ CAPINT + LOGSALE + CURRAT + NFATAST +
                  FATTOT + PAYOUT + WCFTCL, data = uk_train)
summary(mod_sel_lasso)

# Calculating the error
pred_aic_lasso <- predict(mod_sel_lasso, newdata = uk_test)
res_aic_lasso <- pred_aic_lasso - uk_test$RETCAP
error_aic_lasso <- mean(res_aic_lasso^2)

# Final model selection
errors <- c("AIC_Backward" = error_aic_back,
            "AIC_forward" = error_aic_forw,
            "AIC_step" = error_aic_step,
            "Lasso" = error_aic_lasso)

print("Smallest error is ")

