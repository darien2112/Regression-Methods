physical <- read.table("~/path-to-folder/Physical.txt", header=T)
attach(physical)

model.1 <- lm(Height ~ LeftArm + LeftFoot + HeadCirc + nose)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 18.50265    7.83031   2.363   0.0221 *  
# LeftArm      0.80205    0.17074   4.697 2.09e-05 ***
# LeftFoot     0.99730    0.16230   6.145 1.30e-07 ***
# HeadCirc     0.08052    0.14952   0.539   0.5926    
# nose        -0.14740    0.49233  -0.299   0.7659    

plot(fitted(model.1), residuals(model.1),
     panel.last = abline(h=0, lty=2))

anova(model.1)
#           Df Sum Sq Mean Sq  F value    Pr(>F)    
# LeftArm    1 590.21  590.21 123.8106 3.917e-15 ***
# LeftFoot   1 224.35  224.35  47.0621 9.931e-09 ***
# HeadCirc   1   1.40    1.40   0.2940    0.5901    
# nose       1   0.43    0.43   0.0896    0.7659    
# Residuals 50 238.35    4.77                       

model.2 <- lm(Height ~ LeftArm + LeftFoot)
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  21.8572     3.5840   6.098 1.35e-07 ***
# LeftArm       0.7958     0.1652   4.816 1.31e-05 ***
# LeftFoot      1.0229     0.1468   6.969 5.54e-09 ***

anova(model.2)
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# LeftArm    1 590.21  590.21 127.782 1.275e-15 ***
# LeftFoot   1 224.35  224.35  48.572 5.538e-09 ***
# Residuals 52 240.18    4.62                      

(240.18-238.35)/(52-50) / (238.35/50) # F = 0.1919446
pf(0.1919446, 2, 50, lower.tail=F) # p-value = 0.8259579

anova(model.2, model.1)
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     52 240.18                           
# 2     50 238.35  2    1.8289 0.1918 0.8261

model.3 <- lm(Height ~ LeftFoot)
anova(model.3)
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# LeftFoot   1 707.42  707.42  107.95 2.172e-14 ***
# Residuals 53 347.33    6.55                      

(347.33-240.18) / 347.33 # Partial R-squared (LeftArm | LeftFoot) = 0.3084962

detach(physical)
