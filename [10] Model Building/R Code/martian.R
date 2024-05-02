martian <- read.table("~/path-to-data/martian.txt", header=T)
attach(martian)

model.1 <- lm(weight ~ height + water)
summary(model.1)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.220194   0.320978  -3.801  0.00421 ** 
# height       0.283436   0.009142  31.003 1.85e-10 ***
# water        0.111212   0.005748  19.348 1.22e-08 ***
# ---
# Residual standard error: 0.1305 on 9 degrees of freedom

model.2 <- lm(weight ~ height)
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -4.14335    1.75340  -2.363   0.0397 *  
# height       0.38893    0.04543   8.561 6.48e-06 ***
# ---
# Residual standard error: 0.808 on 10 degrees of freedom

plot(x=height, y=weight, col=water/10+1,
     panel.last = c(lines(sort(height[water==0]),
                          fitted(model.1)[water==0][order(height[water==0])],
                          col=1),
                    lines(sort(height[water==10]),
                          fitted(model.1)[water==10][order(height[water==10])],
                          col=2),
                    lines(sort(height[water==20]),
                          fitted(model.1)[water==20][order(height[water==20])],
                          col=3),
                    lines(sort(height), fitted(model.2)[order(height)], col=4)))
legend("topleft", col=1:4, pch=1, lty=1, inset=0.02,
       legend=c("Model 1, water=0", "Model 1, water=10",
                "Model 1, water=20", "Model 2"))

detach(martian)