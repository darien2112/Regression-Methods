statfemales <- read.table("~/path-to-folder/stat_females.txt", header=T)
statfemales <- stat_females
head(statfemales)
attach(statfemales)

pairs(cbind(Height, momheight, dadheight))

model <- lm(Height ~ momheight + dadheight)
summary(model)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 18.54725    3.69278   5.023 1.08e-06 ***
# momheight    0.30351    0.05446   5.573 7.61e-08 ***
# dadheight    0.38786    0.04721   8.216 2.10e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.031 on 211 degrees of freedom
# Multiple R-squared:  0.4335,  Adjusted R-squared:  0.4281 
# F-statistic: 80.73 on 2 and 211 DF,  p-value: < 2.2e-16

plot(fitted(model), residuals(model),
     panel.last = abline(h=0, lty=2))

detach(statfemales)
