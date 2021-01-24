# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2021-01-22

library(dplyr)
library(car)

ble <- read.table('Dataset_Ble.txt', header = TRUE, sep = ";")
summary(ble)
glimpse(ble)

summary(ble$phyto)
table(ble$phyto)

boxplot(ble$rdt~ble$variete)
boxplot(ble$rdt~ble$phyto)

# Checking homscedasticity
leveneTest(ble$rdt, ble$phyto, data = ble)
bartlett.test(ble$rdt, ble$phyto)
# Based on these 2 tests, we can really confirm that we accept H0 which
# means that we accept the homoscedasticity for the factor "Phyto"

leveneTest(ble$rdt, ble$variete, data = ble)
bartlett.test(ble$rdt, ble$variete)
# We cannot accept H0 in this case, both tests are showing a very small p-value
# thus, we reject H0 (not all the variances are the same)

shapiro.test(ble$variete, ble$rdt)

mod1 = lm(ble$rdt~ble$phyto)
mod2 = aov(ble$rdt~ble$phyto)
mod3 = lm(ble$rdt~ble$parcelle)
mod4 = lm(ble$rdt ~ ble$phyto + ble$variete + ble$parcelle + ble$phyto*ble$variete)