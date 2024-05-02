bloodpress <- read.table("~/path-to-data/bloodpress.txt", header=T)
attach(bloodpress)

subset <- regsubsets(BP ~ Age + Weight + BSA + Dur + Pulse + Stress,
                     method="exhaustive", nbest=2, data=bloodpress)
cbind(summary(subset)$outmat, round(summary(subset)$adjr2, 3),
      round(summary(subset)$cp, 1))
#          Age Weight BSA Dur Pulse Stress                
# 1  ( 1 ) " " "*"    " " " " " "   " "    "0.897" "312.8"
# 1  ( 2 ) " " " "    "*" " " " "   " "    "0.736" "829.1"
# 2  ( 1 ) "*" "*"    " " " " " "   " "    "0.99"  "15.1" 
# 2  ( 2 ) " " "*"    " " " " " "   "*"    "0.91"  "256.6"
# 3  ( 1 ) "*" "*"    "*" " " " "   " "    "0.994" "6.4"  
# 3  ( 2 ) "*" "*"    " " " " "*"   " "    "0.991" "14.1" 
# 4  ( 1 ) "*" "*"    "*" "*" " "   " "    "0.994" "6.4"  
# 4  ( 2 ) "*" "*"    "*" " " " "   "*"    "0.994" "7.1"  
# 5  ( 1 ) "*" "*"    "*" " " "*"   "*"    "0.994" "7"    
# 5  ( 2 ) "*" "*"    "*" "*" "*"   " "    "0.994" "7.7"  
# 6  ( 1 ) "*" "*"    "*" "*" "*"   "*"    "0.994" "7"    

model.12 <- lm(BP ~ Age + Weight)
summary(model.12)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -16.57937    3.00746  -5.513 3.80e-05 ***
# Age           0.70825    0.05351  13.235 2.22e-10 ***
# Weight        1.03296    0.03116  33.154  < 2e-16 ***
# ---
# Residual standard error: 0.5327 on 17 degrees of freedom
# Multiple R-squared:  0.9914,  Adjusted R-squared:  0.9904 
# F-statistic: 978.2 on 2 and 17 DF,  p-value: < 2.2e-16

vif(model.12)
#      Age   Weight 
# 1.198945 1.198945 

plot(x=fitted(model.12), y=rstandard(model.12),
     panel.last = abline(h=0, lty=2))

qqnorm(rstandard(model.12), main="", datax=TRUE)
qqline(rstandard(model.12), datax=TRUE)

ad.test(rstandard(model.12)) # A = 0.275, p-value = 0.6225

detach(bloodpress)