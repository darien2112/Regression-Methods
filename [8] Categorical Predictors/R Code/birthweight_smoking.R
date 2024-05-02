birthsmokers <- read.table("~/path-to-folder/birthsmokers.txt", header=T)
attach(birthsmokers)

pairs(cbind(Wgt, Gest, Smoke))

model <- lm(Wgt ~ Gest + Smoke)

plot(x=Gest, y=Wgt, ylim=c(2300, 3700), 
     col=ifelse(Smoke=="yes", "red", "blue"),
     panel.last = c(lines(sort(Gest[Smoke=="no"]),
                          fitted(model)[Smoke=="no"][order(Gest[Smoke=="no"])],
                          col="blue"),
                    lines(sort(Gest[Smoke=="yes"]),
                          fitted(model)[Smoke=="yes"][order(Gest[Smoke=="yes"])],
                          col="red")))

summary(model)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2389.573    349.206  -6.843 1.63e-07 ***
# Gest          143.100      9.128  15.677 1.07e-15 ***
# Smokeyes     -244.544     41.982  -5.825 2.58e-06 ***
# ---
# Residual standard error: 115.5 on 29 degrees of freedom
# Multiple R-squared:  0.8964,  Adjusted R-squared:  0.8892 
# F-statistic: 125.4 on 2 and 29 DF,  p-value: 5.289e-15

confint(model)
#                  2.5 %     97.5 %
# (Intercept) -3103.7795 -1675.3663
# Gest          124.4312   161.7694
# Smokeyes     -330.4064  -158.6817

predict(model, interval="confidence",
        newdata=data.frame(Gest=c(38, 38), Smoke=c("yes", "no")))
#        fit      lwr      upr
# 1 2803.693 2740.599 2866.788
# 2 3048.237 2989.120 3107.355

model.0 <- lm(Wgt ~ Gest, subset=Smoke=="no")
summary(model.0)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2546.14     457.29  -5.568 6.93e-05 ***
# Gest          147.21      11.97  12.294 6.85e-09 ***

predict(model.0, interval="confidence",
        newdata=data.frame(Gest=38))
#        fit      lwr     upr
# 1 3047.724 2990.298 3105.15

model.1 <- lm(Wgt ~ Gest, subset=Smoke=="yes")
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2474.56     553.97  -4.467 0.000532 ***
# Gest          139.03      14.11   9.851 1.12e-07 ***

predict(model.1, interval="confidence",
        newdata=data.frame(Gest=38))
#        fit      lwr      upr
# 1 2808.528 2731.726 2885.331

Smoke2 <- ifelse(Smoke=="yes", 1, -1)
model.3 <- lm(Wgt ~ Gest + Smoke2)
summary(model.3)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2511.845    353.449  -7.107 8.07e-08 ***
# Gest          143.100      9.128  15.677 1.07e-15 ***
# Smoke2       -122.272     20.991  -5.825 2.58e-06 ***

# Alternatively
model.3 <- lm(Wgt ~ Gest + Smoke, contrasts=list(Smoke="contr.sum"))
summary(model.3)

detach(birthsmokers)