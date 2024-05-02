influence4 <- read.table("~/path-to-data/influence4.txt", header=T)
attach(influence4)

plot(x, y)

model.1 <- lm(y ~ x)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   8.5046     4.2224   2.014 0.058374 .  
# x             3.3198     0.6862   4.838 0.000114 ***
# ---
# Residual standard error: 10.45 on 19 degrees of freedom
# Multiple R-squared:  0.5519,  Adjusted R-squared:  0.5284 
# F-statistic: 23.41 on 1 and 19 DF,  p-value: 0.0001143

model.2 <- lm(y ~ x, subset=1:20) # exclude obs #21
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.7322     1.1205   1.546     0.14    
# x             5.1169     0.2003  25.551 1.35e-15 ***
# ---
# Residual standard error: 2.592 on 18 degrees of freedom
# Multiple R-squared:  0.9732,  Adjusted R-squared:  0.9717 
# F-statistic: 652.8 on 1 and 18 DF,  p-value: 1.353e-15

plot(x=x, y=y, col=ifelse(Row<=20, "blue", "red"),
     panel.last = c(lines(sort(x), fitted(model.1)[order(x)], col="red"),
                    lines(sort(x[-21]), fitted(model.2)[order(x[-21])],
                          col="red", lty=2)))
legend("topleft", col="red", lty=c(1,2),
       inset=0.02, legend=c("Red point included", "Red point excluded"))

lev <- hatvalues(model.1)
round(lev, 6)
#        1        2        3        4        5        6        7        8        9       
# 0.158964 0.143985 0.119522 0.113263 0.085774 0.078589 0.067369 0.063924 0.049897
#       10       11       12       13       14       15       16       17       18      
# 0.052019 0.047667 0.048354 0.049990 0.057084 0.058943 0.081446 0.092800 0.101587
#       19       20       21 
# 0.101146 0.116146 0.311532 
sum(lev) # 2

dffit <- dffits(model.1)
round(dffit, 6)
#         1          2          3          4          5          6          7
# -0.402761  -0.243756  -0.205848   0.037612  -0.131355  -0.109593   0.040473
#         8          9         10         11         12         13         14
# -0.042401   0.060224   0.009181   0.005430   0.078165   0.127828   0.007230
#        15         16         17         18         19         20         21
#  0.073067   0.280501   0.323599   0.436114   0.308869   0.249206 -11.467011

cook <- cooks.distance(model.1)
round(cook, 6)
#        1        2        3        4        5        6        7        8
# 0.081718 0.030755 0.021983 0.000746 0.009014 0.006290 0.000863 0.000947
#        9       10       11       12       13       14       15       16
# 0.001907 0.000044 0.000016 0.003203 0.008478 0.000028 0.002804 0.039575
#       17       18       19       20       21
# 0.052293 0.091802 0.048085 0.031938 4.048013

detach(influence4)