mammgest <- read.table("~/path-to-folder/mammgest.txt", header=T)
attach(mammgest)

model.1 <- lm(Gestation ~ Birthwgt)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 187.0837    26.9426   6.944 6.73e-05 ***
# Birthwgt      3.5914     0.5247   6.844 7.52e-05 ***
# Multiple R-squared:  0.8388,  Adjusted R-squared:  0.8209 

plot(x=Birthwgt, y = Gestation,
     panel.first = grid(col = "gray", lty = "dotted"),
     panel.last = lines(sort(Birthwgt), fitted(model.1)[order(Birthwgt)]))

plot(x=fitted(model.1), y=residuals(model.1),
     panel.last = abline(h=0, lty=2))

par(mfrow = c(3,3))
plot(model.1)

par(mfrow = c(1,1))
qqnorm(residuals(model.1), main="", datax=TRUE)
qqline(residuals(model.1), datax=TRUE)

ad.test(residuals(model.1)) # A = 0.3116, p-value = 0.503

lnGest <- log(Gestation)

model.2 <- lm(lnGest ~ Birthwgt)
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 5.278817   0.088177  59.866  5.1e-13 ***
# Birthwgt    0.010410   0.001717   6.062 0.000188 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2163 on 9 degrees of freedom
# Multiple R-squared:  0.8033,  Adjusted R-squared:  0.7814 
# F-statistic: 36.75 on 1 and 9 DF,  p-value: 0.0001878

plot(x=Birthwgt, y=lnGest,
     panel.first = grid(col = "gray", lty = "dotted"),
     panel.last = lines(sort(Birthwgt), fitted(model.2)[order(Birthwgt)]))

plot(x=fitted(model.2), y=residuals(model.2),
     panel.last = abline(h=0, col = "red", lty=2))

qqnorm(residuals(model.2), main="", datax=TRUE)
qqline(residuals(model.2), datax=TRUE)

ad.test(residuals(model.2)) # A = 0.3135, p-value = 0.4963

exp(predict(model.2, interval="prediction",
            newdata=data.frame(Birthwgt=50)))
#        fit      lwr      upr
# 1 330.0781 197.3013 552.2092

# proportional change in median gestation for 1-unit increase in birthwgt
exp(coefficients(model.2)[2]) # 1.010465
exp(confint(model.2)[2,]) # 95% CI
#    2.5 %   97.5 % 
# 1.006547 1.014398 

# proportional change in median gestation for 10-unit increase in birthwgt
exp(10*coefficients(model.2)[2]) # 1.109714
exp(10*confint(model.2)[2,]) # 95% CI
#    2.5 %   97.5 % 
# 1.067429 1.153674 

detach(mammgest)
