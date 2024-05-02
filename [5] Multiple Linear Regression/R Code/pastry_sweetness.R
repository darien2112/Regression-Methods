pastry <- read.table("~/path-to-folder/pastry.txt", header=T)
head(pastry)
attach(pastry)

cor(Sweetness, Moisture) # 0
plot(Sweetness, Moisture)

model.12 <- lm(Rating ~ Moisture + Sweetness)
summary(model.12)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  37.6500     2.9961  12.566 1.20e-08 ***
# Moisture      4.4250     0.3011  14.695 1.78e-09 ***
# Sweetness     4.3750     0.6733   6.498 2.01e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.693 on 13 degrees of freedom
# Multiple R-squared:  0.9521,  Adjusted R-squared:  0.9447 
# F-statistic: 129.1 on 2 and 13 DF,  p-value: 2.658e-09

plot(Moisture, Rating, type="n")
for (i in 1:16) points(Moisture[i], Rating[i], pch=Sweetness[i], col=Sweetness[i])
for (i in c(2,4)) lines(Moisture[Sweetness==i], fitted(model.12)[Sweetness==i],
                        lty=i, col=i)
legend("topleft", legend=c("Sweetness = 4", 
                           "Sweetness = 2"),
       col=c(4,2), pch=c(4,2), inset=0.02)

model.1 <- lm(Rating ~ Moisture)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   50.775      4.395  11.554 1.52e-08 ***
# Moisture       4.425      0.598   7.399 3.36e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.349 on 14 degrees of freedom
# Multiple R-squared:  0.7964,  Adjusted R-squared:  0.7818 
# F-statistic: 54.75 on 1 and 14 DF,  p-value: 3.356e-06

model.2 <- lm(Rating ~ Sweetness)
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   68.625      8.610   7.970 1.43e-06 ***
# Sweetness      4.375      2.723   1.607     0.13    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.89 on 14 degrees of freedom
# Multiple R-squared:  0.1557,  Adjusted R-squared:  0.09539 
# F-statistic: 2.582 on 1 and 14 DF,  p-value: 0.1304

detach(pastry)
