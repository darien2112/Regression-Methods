lungfunction <- read.table("~/path-to-folder/fev_dat.txt", header=T)
attach(lungfunction)

model.1 <- lm(FEV ~ age, subset = age>=6 & age<=10)
summary(model.1)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.01165    0.15237   0.076    0.939    
# age          0.26721    0.01801  14.839   <2e-16 ***

plot(age[age>=6 & age<=10], FEV[age>=6 & age<=10], 
     xlab="Age", ylab="Forced Exhalation Volume (FEV)",
     panel.last = lines(sort(age[age>=6 & age<=10]), 
                        fitted(model.1)[order(age[age>=6 & age<=10])]))

model.2 <- lm(FEV ~ age)
summary(model.2)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.431648   0.077895   5.541 4.36e-08 ***
# age         0.222041   0.007518  29.533  < 2e-16 ***

plot(age, FEV, xlab="Age", ylab="Forced Exhalation Volume (FEV)",
     panel.last = lines(sort(age), fitted(model.2)[order(age)]))
detach(lungfunction)