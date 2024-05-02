coolhearts <- read.table("~/path-to-folder/coolhearts.txt", header=T)
attach(coolhearts)
head(coolhearts)

model.1 <- lm(Infarc ~ Area + X2 + X3)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.13454    0.10402  -1.293 0.206459    
# Area         0.61265    0.10705   5.723 3.87e-06 ***
# X2          -0.24348    0.06229  -3.909 0.000536 ***
# X3          -0.06566    0.06507  -1.009 0.321602    

plot(Area, Infarc, type="n", ylim=c(-0.2, 1),
     xlab="Size of area at risk (grams)",
     ylab="Size of infarcted area (grams)")
for (i in 1:32) points(Area[i], Infarc[i], pch=Group[i], col=Group[i])
for (i in 1:3) lines(Area[Group==i], fitted(model.1)[Group==i], lty=i, col=i)
legend("topleft", legend=c("no cooling", 
                           "late cooling",
                           "early cooling"),
       col=3:1, pch=3:1, inset=0.02)

