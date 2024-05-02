realestate <- read.table("~/path-to-folder/realestate.txt", header=T)
attach(realestate)

SqFeet.Air <- SqFeet*Air
model.1 <- lm(SalePrice ~ SqFeet + Air + SqFeet.Air)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -3.218     30.085  -0.107 0.914871    
# SqFeet       104.902     15.748   6.661 6.96e-11 ***
# Air          -78.868     32.663  -2.415 0.016100 *  
# SqFeet.Air    55.888     16.580   3.371 0.000805 ***

plot(x=SqFeet, y=SalePrice, 
     col=ifelse(Air, "red", "blue"),
     panel.last = c(lines(sort(SqFeet[Air==0]),
                          fitted(model.1)[Air==0][order(SqFeet[Air==0])],
                          col="blue"),
                    lines(sort(SqFeet[Air==1]),
                          fitted(model.1)[Air==1][order(SqFeet[Air==1])],
                          col="red")))
legend("topleft", col=c("blue", "red"), pch=1, lty=1, inset=0.02,
       legend=c("No air conditioning", "Air conditioning"))

plot(x=fitted(model.1), y=residuals(model.1),
     xlab="Fitted values", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

lnSalePrice <- log(SalePrice)
lnSqFeet <- log(SqFeet)
lnSqFeet.Air <- lnSqFeet*Air

model.2 <- lm(lnSalePrice ~ lnSqFeet + Air + lnSqFeet.Air)

plot(x=lnSqFeet, y=lnSalePrice, 
     col=ifelse(Air, "red", "blue"),
     panel.last = c(lines(sort(lnSqFeet[Air==0]),
                          fitted(model.2)[Air==0][order(lnSqFeet[Air==0])],
                          col="blue"),
                    lines(sort(lnSqFeet[Air==1]),
                          fitted(model.2)[Air==1][order(lnSqFeet[Air==1])],
                          col="red")))
legend("topleft", col=c("blue", "red"), pch=1, lty=1, inset=0.02,
       legend=c("No air conditioning", "Air conditioning"))

plot(x=fitted(model.2), y=residuals(model.2),
     xlab="Fitted values", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

detach(realestate)