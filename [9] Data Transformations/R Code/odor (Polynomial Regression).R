odor <- read.table("~/path-to-folder/odor.txt", header=T)
attach(odor)

Tempsq <- Temp^2
Ratiosq <- Ratio^2
Heightsq <- Height^2

model.1 <- lm(Odor ~ Temp + Ratio + Height + Tempsq + Ratiosq + Heightsq)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -30.667     10.840  -2.829   0.0222 * 
# Temp         -12.125      6.638  -1.827   0.1052   
# Ratio        -17.000      6.638  -2.561   0.0336 * 
# Height       -21.375      6.638  -3.220   0.0122 * 
# Tempsq        32.083      9.771   3.284   0.0111 * 
# Ratiosq       47.833      9.771   4.896   0.0012 **
# Heightsq       6.083      9.771   0.623   0.5509   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 18.77 on 8 degrees of freedom
# Multiple R-squared:  0.8683,  Adjusted R-squared:  0.7695 
# F-statistic: 8.789 on 6 and 8 DF,  p-value: 0.003616

model.2 <- lm(Odor ~ Temp + Ratio + Height + Tempsq + Ratiosq)
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -26.923      8.707  -3.092 0.012884 *  
# Temp         -12.125      6.408  -1.892 0.091024 .  
# Ratio        -17.000      6.408  -2.653 0.026350 *  
# Height       -21.375      6.408  -3.336 0.008720 ** 
# Tempsq        31.615      9.404   3.362 0.008366 ** 
# Ratiosq       47.365      9.404   5.036 0.000703 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 18.12 on 9 degrees of freedom
# Multiple R-squared:  0.8619,  Adjusted R-squared:  0.7852 
# F-statistic: 11.23 on 5 and 9 DF,  p-value: 0.001169

detach(odor)