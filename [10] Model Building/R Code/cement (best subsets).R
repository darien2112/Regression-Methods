cement <- read.table("~/path-to-data/cement.txt", header=T)
attach(cement)

library(leaps)

subset <- regsubsets(y ~ x1 + x2 + x3 + x4, method="exhaustive", nbest=2, data=cement)
cbind(summary(subset)$outmat, round(summary(subset)$adjr2, 3), round(summary(subset)$cp, 1))
#          x1  x2  x3  x4                 
# 1  ( 1 ) " " " " " " "*" "0.645" "138.7"
# 1  ( 2 ) " " "*" " " " " "0.636" "142.5"
# 2  ( 1 ) "*" "*" " " " " "0.974" "2.7"  
# 2  ( 2 ) "*" " " " " "*" "0.967" "5.5"  
# 3  ( 1 ) "*" "*" " " "*" "0.976" "3"    
# 3  ( 2 ) "*" "*" "*" " " "0.976" "3"    
# 4  ( 1 ) "*" "*" "*" "*" "0.974" "5"    

model.1234 <- lm(y ~ x1 + x2 + x3 + x4)
model.12 <- lm(y ~ x1 + x2)

SSE.k <- sum(residuals(model.12)^2) # SSE_k = 57.90448
MSE.all <- summary(model.1234)$sigma^2 # MSE_all = 5.982955
params <- summary(model.12)$df[1] # k+1 = 3
n <- sum(summary(model.1234)$df[1:2]) # n = 13
SSE.k/MSE.all + 2*params - n # Cp = 2.678242

model.14 <- lm(y ~ x1 + x4)

SSE.k <- sum(residuals(model.14)^2) # SSE_k = 74.76211
params <- summary(model.14)$df[1] # k+1 = 3
SSE.k/MSE.all + 2*params - n # Cp = 5.495851

model.124 <- lm(y ~ x1 + x2 + x4)
library(car)
vif(model.124)
#      x1       x2       x4 
# 1.06633 18.78031 18.94008 

model.123 <- lm(y ~ x1 + x2 + x3)
vif(model.123)
#       x1       x2       x3 
# 3.251068 1.063575 3.142125 

summary(model.12)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 52.57735    2.28617   23.00 5.46e-10 ***
# x1           1.46831    0.12130   12.11 2.69e-07 ***
# x2           0.66225    0.04585   14.44 5.03e-08 ***
# ---
# Residual standard error: 2.406 on 10 degrees of freedom
# Multiple R-squared:  0.9787,  Adjusted R-squared:  0.9744 
# F-statistic: 229.5 on 2 and 10 DF,  p-value: 4.407e-09

vif(model.12)
#       x1       x2 
# 1.055129 1.055129 

plot(x=fitted(model.12), y=rstandard(model.12),
     panel.last = abline(h=0, lty=2))

qqnorm(rstandard(model.12), main="", datax=TRUE)
qqline(rstandard(model.12), datax=TRUE)

library(nortest)
ad.test(rstandard(model.12)) # A = 0.6136, p-value = 0.08628

detach(cement)