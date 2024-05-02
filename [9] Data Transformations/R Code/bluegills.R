bluegills <- read.table("~/path-to-folder/bluegills.txt", header=T)
attach(bluegills)

plot(x=age, y=length)

agesq <- age^2

model <- lm(length ~ age + agesq)
summary(model)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   13.622     11.016   1.237     0.22    
# age           54.049      6.489   8.330 2.81e-12 ***
# agesq         -4.719      0.944  -4.999 3.67e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.91 on 75 degrees of freedom
# Multiple R-squared:  0.8011,  Adjusted R-squared:  0.7958 
# F-statistic: 151.1 on 2 and 75 DF,  p-value: < 2.2e-16

newX <- seq(min(age), max(age), length=100)
newX
newXsq <- newX**2

plot(x=age, y=length,
     panel.last = lines(newX,
                        predict(model,
                                newdata=data.frame(age=newX, agesq=newXsq))))

predict(model, interval="prediction",
        newdata=data.frame(age=5, agesq=25))
#        fit     lwr      upr
# 1 165.9023 143.487 188.3177

detach(bluegills)
