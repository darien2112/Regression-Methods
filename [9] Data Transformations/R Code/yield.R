yield <- read.table("~/path-to-folder/yield.txt", header=T)
attach(yield)

model.1 <- lm(Yield ~ Temp)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.306306   0.469075   4.917 0.000282 ***
# Temp        0.006757   0.005873   1.151 0.270641    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3913 on 13 degrees of freedom
# Multiple R-squared:  0.09242,  Adjusted R-squared:  0.0226 
# F-statistic: 1.324 on 1 and 13 DF,  p-value: 0.2706

plot(x=Temp, y=Yield,
     panel.last = lines(sort(Temp), fitted(model.1)[order(Temp)]))

Tempsq <- Temp^2

model.2 <- lm(Yield ~ Temp + Tempsq)
summary(model.2)
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.9604811  1.2589183   6.323 3.81e-05 ***
# Temp        -0.1537113  0.0349408  -4.399 0.000867 ***
# Tempsq       0.0010756  0.0002329   4.618 0.000592 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2444 on 12 degrees of freedom
# Multiple R-squared:  0.6732,  Adjusted R-squared:  0.6187 
# F-statistic: 12.36 on 2 and 12 DF,  p-value: 0.001218

newX <- seq(min(Temp), max(Temp), length=100)
newXsq <- newX**2

plot(x=Temp, y=Yield,
     panel.last = lines(newX,
                        predict(model.2,
                                newdata=data.frame(Temp=newX, Tempsq=newXsq))))

detach(yield)