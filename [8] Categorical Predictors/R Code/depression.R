depression <- read.table("~/path-to-folder/depression.txt", header=T)
attach(depression)

plot(x=age, y=y, col=as.numeric(TRT))
legend("topleft", col=1:3, pch=1,
       inset=0.02, x.intersp = 1.5, y.intersp = 1.8,
       legend=c("Trt A", "Trt B", "Trt C"))

age.x2 <- age*x2
age.x3 <- age*x3

model.1 <- lm(y ~ age + x2 + x3 + age.x2 + age.x3)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.21138    3.34964   1.854 0.073545 .  
# age          1.03339    0.07233  14.288 6.34e-15 ***
# x2          41.30421    5.08453   8.124 4.56e-09 ***
# x3          22.70682    5.09097   4.460 0.000106 ***
# age.x2      -0.70288    0.10896  -6.451 3.98e-07 ***
# age.x3      -0.50971    0.11039  -4.617 6.85e-05 ***

plot(x=age, y=y, ylim=c(20, 80), col=as.numeric(TRT),
     panel.last = c(lines(sort(age[TRT=="A"]),
                          fitted(model.1)[TRT=="A"][order(age[TRT=="A"])],
                          col=1),
                    lines(sort(age[TRT=="B"]),
                          fitted(model.1)[TRT=="B"][order(age[TRT=="B"])],
                          col=2),
                    lines(sort(age[TRT=="C"]),
                          fitted(model.1)[TRT=="C"][order(age[TRT=="C"])],
                          col=3)))
legend("topleft", col=1:3, pch=1, lty=1,
       inset=0.02, x.intersp = 1.5, y.intersp = 1.8,
       legend=c("Trt A", "Trt B", "Trt C"))

plot(x=fitted(model.1), y=rstudent(model.1),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.1), main="", datax=TRUE)
qqline(residuals(model.1), datax=TRUE)

library(nortest)
ad.test(residuals(model.1)) # A = 0.4057, p-value = 0.3345

anova(model.1)
#           Df Sum Sq Mean Sq  F value    Pr(>F)    
# age        1 3424.4  3424.4 222.2946 2.059e-15 ***
# x2         1  803.8   803.8  52.1784 4.857e-08 ***
# x3         1    1.2     1.2   0.0772    0.7831    
# age.x2     1  375.0   375.0  24.3430 2.808e-05 ***
# age.x3     1  328.4   328.4  21.3194 6.850e-05 ***
# Residuals 30  462.1    15.4                       

model.2 <- lm(y ~ age)
anova(model.2, model.1)
# Model 1: y ~ age
# Model 2: y ~ age + x2 + x3 + age.x2 + age.x3
#   Res.Df     RSS Df Sum of Sq     F    Pr(>F)    
# 1     34 1970.57                                 
# 2     30  462.15  4    1508.4 24.48 4.458e-09 ***

model.3 <- lm(y ~ age + x2 + x3)
anova(model.3, model.1)
# Model 1: y ~ age + x2 + x3
# Model 2: y ~ age + x2 + x3 + age.x2 + age.x3
#   Res.Df     RSS Df Sum of Sq      F   Pr(>F)    
# 1     32 1165.57                                 
# 2     30  462.15  2    703.43 22.831 9.41e-07 ***

detach(depression)