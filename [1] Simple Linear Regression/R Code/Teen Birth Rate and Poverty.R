poverty <- read.table("~/path-to-folder/poverty.txt", header=T)
attach(poverty)

model <- lm(Brth15to17 ~ PovPct)
summary(model)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.2673     2.5297   1.687    0.098 .  
# PovPct        1.3733     0.1835   7.483 1.19e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.551 on 49 degrees of freedom
# Multiple R-squared:  0.5333,  Adjusted R-squared:  0.5238 

plot(PovPct, Brth15to17, xlab="Poverty Rate", ylab="15 to 17 Year Old Birth Rate",
     panel.last = lines(sort(PovPct), fitted(model)[order(PovPct)]))
detach(poverty)

