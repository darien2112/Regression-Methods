shortleaf <- read.table("~/path-to-folder/shortleaf.txt", header=T)
attach(shortleaf)

model.1 <- lm(Vol ~ Diam)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -41.5681     3.4269  -12.13   <2e-16 ***
# Diam          6.8367     0.2877   23.77   <2e-16 ***
# ---
# Multiple R-squared:  0.8926,  Adjusted R-squared:  0.891 

plot(x=Diam, y=Vol,
     panel.first = grid(col = "gray", lty = "dotted"),
     panel.last = lines(sort(Diam), fitted(model.1)[order(Diam)]))

plot(x=fitted(model.1), y=residuals(model.1),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.1), main="", datax=TRUE)
qqline(residuals(model.1), datax=TRUE)

par(mfrow = c(2,2))
plot(model.1)

ad.test(residuals(model.1)) # A = 0.9913, p-value = 0.01215

lnDiam <- log(Diam)

model.2 <- lm(Vol ~ lnDiam)
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -116.162     10.830  -10.73 2.88e-16 ***
# lnDiam        64.536      4.562   14.15  < 2e-16 ***
# Multiple R-squared:  0.7464,  Adjusted R-squared:  0.7427 

plot(x=lnDiam, y=Vol,
     panel.last = lines(sort(lnDiam), fitted(model.2)[order(lnDiam)]))

plot(x=fitted(model.2), y=residuals(model.2),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.2), main="", datax=TRUE)
qqline(residuals(model.2), datax=TRUE)

ad.test(residuals(model.2)) # A = 2.3845, p-value = 4.273e-06

lnVol <- log(Vol)

model.3 <- lm(lnVol ~ lnDiam)
summary(model.3)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2.8718     0.1216  -23.63   <2e-16 ***
# lnDiam        2.5644     0.0512   50.09   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1703 on 68 degrees of freedom
# Multiple R-squared:  0.9736,  Adjusted R-squared:  0.9732 
# F-statistic:  2509 on 1 and 68 DF,  p-value: < 2.2e-16

plot(x=lnDiam, y=lnVol,
     panel.last = lines(sort(lnDiam), fitted(model.3)[order(lnDiam)]))

plot(x=fitted(model.3), y=residuals(model.3),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.3), main="", datax=TRUE)
qqline(residuals(model.3), datax=TRUE)

ad.test(residuals(model.3)) # A = 0.5309, p-value = 0.1692

exp(predict(model.3, interval="confidence",
            newdata=data.frame(lnDiam=log(10))))
#        fit      lwr      upr
# 1 20.75934 19.92952 21.62372

# proportional change in median Vol for 2-fold increase in Diam
2^(coefficients(model.3)[2]) # 5.915155
2^(confint(model.3)[2,]) # 95% CI
#    2.5 %   97.5 % 
# 5.510776 6.349207

detach(shortleaf)
