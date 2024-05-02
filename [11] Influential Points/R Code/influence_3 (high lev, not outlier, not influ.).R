influence3 <- read.table("~/path-to-data/influence3.txt", header=T)
attach(influence3)

plot(x, y)

model.1 <- lm(y ~ x)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.4679     1.0757   2.294   0.0333 *  
# x             4.9272     0.1719  28.661   <2e-16 ***
# ---
# Residual standard error: 2.709 on 19 degrees of freedom
# Multiple R-squared:  0.9774,  Adjusted R-squared:  0.9762 
# F-statistic: 821.4 on 1 and 19 DF,  p-value: < 2.2e-16

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
# 0.153481 0.139367 0.116292 0.110382 0.084374 0.077557 0.066879 0.063589 0.050033
#       10       11       12       13       14       15       16       17       18      
# 0.052121 0.047632 0.048156 0.049557 0.055893 0.057574 0.078121 0.088549 0.096634
#       19       20       21 
# 0.096227 0.110048 0.357535 
sum(lev) # 2

dffit <- dffits(model.1)
round(dffit, 6)
#         1         2         3         4         5         6         7         8
# -0.525036 -0.083882 -0.182326  0.758981 -0.218230 -0.201548  0.277728 -0.082294
#         9        10        11        12        13        14        15        16
#  0.138643 -0.022210 -0.184873  0.055235  0.197411 -0.424484 -0.172490  0.299173
#        17        18        19        20        21
#  0.309606  0.630493  0.149474 -0.250945 -1.238416

cook <- cooks.distance(model.1)
round(cook, 6)
#        1        2        3        4        5        6        7        8
# 0.134157 0.003705 0.017302 0.241690 0.024433 0.020879 0.038412 0.003555
#        9       10       11       12       13       14       15       16
# 0.009943 0.000260 0.017379 0.001605 0.019748 0.081344 0.015289 0.044620
#       17       18       19       20       21
# 0.047961 0.173901 0.011656 0.032322 0.701965

detach(influence3)