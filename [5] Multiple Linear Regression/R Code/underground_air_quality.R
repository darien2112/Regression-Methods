babybirds <- read.table("~/path-to-folder/babybirds.txt", header=T)
attach(babybirds)

pairs(cbind(Vent, O2, CO2))

library(car)
scatter3d(Vent ~ O2 + CO2)
scatter3d(Vent ~ O2 + CO2, revolutions=3, speed=0.5, grid=F)

model <- lm(Vent ~ O2 + CO2)
summary(model)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   85.901    106.006   0.810    0.419    
# O2            -5.330      6.425  -0.830    0.408    
# CO2           31.103      4.789   6.495  2.1e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 157.4 on 117 degrees of freedom
# Multiple R-squared:  0.2682,  Adjusted R-squared:  0.2557 
# F-statistic: 21.44 on 2 and 117 DF,  p-value: 1.169e-08

Anova(model, type="III") # Adjusted (type III) SS
# Anova Table (Type III tests)
# Response: Vent
#              Sum Sq  Df F value    Pr(>F)    
# (Intercept)   16262   1  0.6566    0.4194    
# O2            17045   1  0.6883    0.4084    
# CO2         1044773   1 42.1866 2.104e-09 ***
# Residuals   2897566 117                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

detach(babybirds)
