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
# Plotting the histogram
hist(data$V1, freq = FALSE)
# Based on the histogream, it looks like an Uniform distribution
# Applying the Kolmogorov-Smirnov test
ks.test(data$V1, punif, min(data$V1), max(data$V1))
# p-value: 0.783 ==> H0 is not rejected, data$V1 seems to be a uniform
# random variable

# Option 2
# Calculating the time arrival between 2 points
data_diff <- diff(data$V1)
# Histogram of the time difference between 2 points from data$V1
hist(data_diff, freq = FALSE)
# Based on the histogream, it looks like an Exponential distribution
# Applying the Kolmogorov-Smirnov test
ks.test(data_diff, pexp, 1 / mean(data_diff))
# p-value: 0.5568 ==> We accept H0, the difference between 2 points of data$V1
# is an exponential random variable

# Calculating lambda
lambda_hat <- 1 / mean(data_diff)
lambda_hat



