iqsize <- read.table("~/path-to-data/iqsize.txt", header=T)
attach(iqsize)

subset <- regsubsets(PIQ ~ Brain + Height + Weight, method="exhaustive", nbest=2, data=iqsize)
cbind(summary(subset)$outmat, round(summary(subset)$adjr2, 3), round(summary(subset)$cp, 1))
#          Brain Height Weight                
# 1  ( 1 ) "*"   " "    " "    "0.119"  "7.3" 
# 1  ( 2 ) " "   "*"    " "    "-0.019" "13.8"
# 2  ( 1 ) "*"   "*"    " "    "0.255"  "2"   
# 2  ( 2 ) "*"   " "    "*"    "0.146"  "6.9" 
# 3  ( 1 ) "*"   "*"    "*"    "0.233"  "4"   

model.12 <- lm(PIQ ~ Brain + Height)
summary(model.12)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 111.2757    55.8673   1.992 0.054243 .  
# Brain         2.0606     0.5466   3.770 0.000604 ***
# Height       -2.7299     0.9932  -2.749 0.009399 ** 
# ---
# Residual standard error: 19.51 on 35 degrees of freedom
# Multiple R-squared:  0.2949,  Adjusted R-squared:  0.2546 
# F-statistic: 7.321 on 2 and 35 DF,  p-value: 0.002208

vif(model.12)
#    Brain   Height 
# 1.529463 1.529463 

plot(x=fitted(model.12), y=rstandard(model.12),
     panel.last = abline(h=0, lty=2))

qqnorm(rstandard(model.12), main="", datax=TRUE)
qqline(rstandard(model.12), datax=TRUE)

ad.test(rstandard(model.12)) # A = 0.2629, p-value = 0.6829

detach(iqsize)