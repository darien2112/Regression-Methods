wordrecall <- read.table("~/path-to-folder/wordrecall.txt", header=T)
attach(wordrecall)

model.1 <- lm(prop ~ time)
summary(model.1)
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.259e-01  4.881e-02  10.774 3.49e-07 ***
# time        -5.571e-05  1.457e-05  -3.825  0.00282 ** 
# Multiple R-squared:  0.5709,  Adjusted R-squared:  0.5318 

plot(x=time, y=prop, ylim=c(-0.1, 0.9),
     panel.last = lines(sort(time), fitted(model.1)[order(time)]))

plot(x=fitted(model.1), y=residuals(model.1),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.1), main="", datax=TRUE)
qqline(residuals(model.1), datax=TRUE)

library(nortest)
ad.test(residuals(model.1)) # A = 0.262, p-value = 0.6426

lntime <- log(time)

model.2 <- lm(prop ~ lntime)

summary(model.2)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.846415   0.014195   59.63 3.65e-15 ***
# lntime      -0.079227   0.002416  -32.80 2.53e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02339 on 11 degrees of freedom
# Multiple R-squared:  0.9899,  Adjusted R-squared:  0.989 
# F-statistic:  1076 on 1 and 11 DF,  p-value: 2.525e-12

plot(x=lntime, y=prop,
     panel.last = lines(sort(lntime), fitted(model.2)[order(lntime)]))

plot(x=fitted(model.2), y=residuals(model.2),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.2), main="", datax=TRUE)
qqline(residuals(model.2), datax=TRUE)

ad.test(residuals(model.2)) # A = 0.3216, p-value = 0.4869

prop1.25 <- prop^-1.25

model.3 <- lm(prop1.25 ~ time)
summary(model.3)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.8693698  0.3869678   4.831 0.000527 ***
# time        0.0019708  0.0001155  17.067 2.91e-09 ***
# Multiple R-squared:  0.9636,  Adjusted R-squared:  0.9603 

plot(x=time, y=prop1.25,
     panel.last = lines(sort(time), fitted(model.3)[order(time)]))

plot(x=fitted(model.3), y=residuals(model.3),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.3), main="", datax=TRUE)
qqline(residuals(model.3), datax=TRUE)

ad.test(residuals(model.3)) # A = 1.191, p-value = 0.002584

predict(model.2, interval="prediction",
        newdata=data.frame(lntime=log(1000)))
#         fit       lwr       upr
# 1 0.2991353 0.2449729 0.3532978

confint(model.2)[2,]*log(10) # 95% CI for 10-fold increase in time
#      2.5 %     97.5 % 
# -0.1946689 -0.1701845 

detach(wordrecall)
