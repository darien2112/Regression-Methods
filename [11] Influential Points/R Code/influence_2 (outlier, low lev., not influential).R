influence2 <- read.table("~/path-to-data/influence2.txt", header=T)
attach(influence2)

plot(x, y)

model.1 <- lm(y ~ x)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.9576     2.0091   1.472    0.157    
# x             5.0373     0.3633  13.865 2.18e-11 ***
# ---
# Residual standard error: 4.711 on 19 degrees of freedom
# Multiple R-squared:  0.9101,  Adjusted R-squared:  0.9053 
# F-statistic: 192.2 on 1 and 19 DF,  p-value: 2.179e-11

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
# 0.176297 0.157454 0.127015 0.119313 0.086145 0.077744 0.065028 0.061276 0.048147
#       10       11       12       13       14       15       16       17       18      
# 0.049628 0.049313 0.051829 0.055760 0.069310 0.072580 0.109616 0.127489 0.141136
#       19       20       21 
# 0.140453 0.163492 0.050974 
sum(lev) # 2

sta <- rstandard(model.1)
round(sta, 6)
#         1         2         3         4         5         6         7         8
# -0.826351 -0.249154 -0.435445  0.998187 -0.581904 -0.574462  0.413791 -0.371226
#         9        10        11        12        13        14        15        16
#  0.139767 -0.262514 -0.713173 -0.095897  0.252734 -1.229353 -0.683161  0.292644
#        17        18        19        20        21
#  0.262144  0.731458 -0.055615 -0.776800  3.681098

stu <- rstudent(model.1)
round(stu, 6)
#         1         2         3         4         5         6         7         8
# -0.819167 -0.242905 -0.425962  0.998087 -0.571499 -0.564060  0.404582 -0.362643
#         9        10        11        12        13        14        15        16
#  0.136110 -0.255977 -0.703633 -0.093362  0.246408 -1.247195 -0.673261  0.285483
#        17        18        19        20        21
#  0.255615  0.722190 -0.054136 -0.768382  6.690129

dffit <- dffits(model.1)
round(dffit, 6)
#         1         2         3         4         5         6         7         8
# -0.378974 -0.105007 -0.162478  0.367368 -0.175466 -0.163769  0.106698 -0.092652
#         9        10        11        12        13        14        15        16
#  0.030612 -0.058495 -0.160254 -0.021828  0.059879 -0.340354 -0.188345  0.100168
#        17        18        19        20        21
#  0.097710  0.292757 -0.021884 -0.339696  1.550500

cook <- cooks.distance(model.1)
round(cook, 6)
#        1        2        3        4        5        6        7        8
# 0.073076 0.005800 0.013794 0.067493 0.015960 0.013909 0.005954 0.004498
#        9       10       11       12       13       14       15       16
# 0.000494 0.001799 0.013191 0.000251 0.001886 0.056275 0.018262 0.005272
#       17       18       19       20       21
# 0.005021 0.043960 0.000253 0.058968 0.363914

detach(influence2)