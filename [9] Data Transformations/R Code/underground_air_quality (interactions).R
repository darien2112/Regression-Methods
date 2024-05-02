swallows <- read.table("~/path-to-folder/allswallows.txt", header=T)
swallows <- allswallows
attach(swallows)

library(car)
scatter3d(Vent ~ O2 + CO2, subset=Type==1) # adult
scatter3d(Vent ~ O2 + CO2, subset=Type==0) # nestling
scatter3d(Vent ~ O2 + CO2, subset=Type==0, revolutions=3, speed=0.5, grid=F)

TypeO2 <- Type*O2
TypeCO2 <- Type*CO2
CO2O2 <- CO2*O2

model.1 <- lm(Vent ~ O2 + CO2 + Type + TypeO2 + TypeCO2 + CO2O2)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  -18.399    160.007  -0.115   0.9086  
# O2             1.189      9.854   0.121   0.9041  
# CO2           54.281     25.987   2.089   0.0378 *
# Type         111.658    157.742   0.708   0.4797  
# TypeO2        -7.008      9.560  -0.733   0.4642  
# TypeCO2        2.311      7.126   0.324   0.7460  
# CO2O2         -1.449      1.593  -0.909   0.3642  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 165.6 on 233 degrees of freedom
# Multiple R-squared:  0.272,  Adjusted R-squared:  0.2533 
# F-statistic: 14.51 on 6 and 233 DF,  p-value: 4.642e-14

anova(model.1) # Sequential (type I) SS
#            Df  Sum Sq Mean Sq F value  Pr(>F)    
# O2          1   93651   93651  3.4156 0.06585 .  
# CO2         1 2247696 2247696 81.9762 < 2e-16 ***
# Type        1    5910    5910  0.2156 0.64288    
# TypeO2      1   14735   14735  0.5374 0.46425    
# TypeCO2     1    2884    2884  0.1052 0.74598    
# CO2O2       1   22664   22664  0.8266 0.36421    
# Residuals 233 6388603   27419                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

((14735+2884+22664)/3)/27419 # F-stat = 0.4897212
pf(0.49, 3, 233, lower.tail=F) # p-value = 0.6895548

plot(x=fitted(model.1), y=residuals(model.1),
     panel.last = abline(h=0, lty=2))

model.2 <- lm(Vent ~ O2 + CO2 + Type)
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  136.767     79.334   1.724    0.086 .  
# O2            -8.834      4.765  -1.854    0.065 .  
# CO2           32.258      3.551   9.084   <2e-16 ***
# Type           9.925     21.308   0.466    0.642    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 165 on 236 degrees of freedom
# Multiple R-squared:  0.2675,  Adjusted R-squared:  0.2581 
# F-statistic: 28.72 on 3 and 236 DF,  p-value: 7.219e-16

plot(x=fitted(model.2), y=residuals(model.2),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.2), main="", datax=TRUE)
qqline(residuals(model.2), datax=TRUE)

ad.test(residuals(model.2)) # A = 0.3175, p-value = 0.5358

detach(swallows)
