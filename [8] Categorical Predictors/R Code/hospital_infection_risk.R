infectionrisk <- read.table("~/path-to-folder/infectionrisk.txt", header=T)
infectionrisk <- infectionrisk[infectionrisk$Stay<=14,]
attach(infectionrisk)

i1 <- ifelse(Region==1,1,0) # NE
i2 <- ifelse(Region==2,1,0) # NC
i3 <- ifelse(Region==3,1,0) # S
i4 <- ifelse(Region==4,1,0) # W

model.1 <- lm(InfctRsk ~ Stay + Xray + i2 + i3 + i4)
summary(model.1)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.134259   0.877347  -2.433  0.01668 *  
# Stay         0.505394   0.081455   6.205 1.11e-08 ***
# Xray         0.017587   0.005649   3.113  0.00238 ** 
# i2           0.171284   0.281475   0.609  0.54416    
# i3           0.095461   0.288852   0.330  0.74169    
# i4           1.057835   0.378077   2.798  0.00612 ** 
# ---
# Residual standard error: 1.036 on 105 degrees of freedom
# Multiple R-squared:  0.4198,  Adjusted R-squared:  0.3922 
# F-statistic: 15.19 on 5 and 105 DF,  p-value: 3.243e-11

model.2 <- lm(InfctRsk ~ Stay + Xray)
anova(model.2, model.1)
#   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1    108 123.56                              
# 2    105 112.71  3    10.849 3.3687 0.02135 *

model.3 <- lm(InfctRsk ~ Stay + Xray + i4)
anova(model.3, model.1)
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    107 113.11                           
# 2    105 112.71  2   0.39949 0.1861 0.8305

detach(infectionrisk)