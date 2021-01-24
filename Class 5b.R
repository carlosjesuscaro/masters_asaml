# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-19

load('arbre.RData')
# Assuming that we comply with all the basic conditions, we
# can proceed with building the model

L = lm(arbre$hauteur~arbre$hetraie, data=arbre)
anova(L)
# based om the anova result, we can conclude that hetrai has an effect on
# heuteu
summary(L)
# based on the summary, we see that hetraie2 has a high p-value which
# means that we keep H0 so we understand that hetraie2 is equal to hetraie1
pairwise.t.test(arbre$hauteur,arbre$hetraie,p.adjust="bonf")
# this confirms that level 1 and 2 are the same and have the same effect

#Comparing by pairs:
TukeyHSD(aov((arbre$hauteur~arbre$hetraie)))