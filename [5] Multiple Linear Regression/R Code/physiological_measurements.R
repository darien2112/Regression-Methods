bodyfat <- read.table("~/path-to-folder/bodyfat.txt", header=T)
attach(bodyfat)

model <- lm(Bodyfat ~ Triceps + Thigh + Midarm, x=T)
summary(model)
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  117.085     99.782   1.173    0.258
# Triceps        4.334      3.016   1.437    0.170
# Thigh         -2.857      2.582  -1.106    0.285
# Midarm        -2.186      1.595  -1.370    0.190
# 
# Residual standard error: 2.48 on 16 degrees of freedom
# Multiple R-squared:  0.8014,  Adjusted R-squared:  0.7641 
# F-statistic: 21.52 on 3 and 16 DF,  p-value: 7.343e-06

anova(model)
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# Triceps    1 352.27  352.27 57.2768 1.131e-06 ***
# Thigh      1  33.17   33.17  5.3931   0.03373 *  
# Midarm     1  11.55   11.55  1.8773   0.18956    
# Residuals 16  98.40    6.15                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

MSE <- sum(residuals(model)^2)/model$df.residual # 6.150306
X <- model$x
XTXinv <- solve(t(X) %*% X)
#             (Intercept)    Triceps       Thigh      Midarm
# (Intercept)  1618.86721 48.8102522 -41.8487041 -25.7987855
# Triceps        48.81025  1.4785133  -1.2648388  -0.7785022
# Thigh         -41.84870 -1.2648388   1.0839791   0.6657581
# Midarm        -25.79879 -0.7785022   0.6657581   0.4139009

sqrt(MSE*diag(XTXinv)) # standard errors of the regression parameters
# (Intercept)     Triceps       Thigh      Midarm 
#   99.782403    3.015511    2.582015    1.595499 

MSE*XTXinv[2,3] # cov(b1, b2) = -7.779145
XTXinv[2,3]/sqrt(XTXinv[2,2]*XTXinv[3,3]) # cor(b1, b2) = -0.9991072

detach(bodyfat)
