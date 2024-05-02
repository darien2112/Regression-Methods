toluca <- read.table("~/path-to-folder/toluca.txt", header=T)
attach(toluca)

model <- lm(WorkHours ~ LotSize)
lotgroup <- factor(LotSize<=70)

library(car)
leveneTest(residuals(model), group=lotgroup)
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  1  1.7331  0.201
#       23               

ncvTest(model)
# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 0.8209192    Df = 1     p = 0.3649116 

detach(toluca)