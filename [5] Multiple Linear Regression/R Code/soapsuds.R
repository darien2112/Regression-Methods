soapsuds <- read.table("~/path-to-folder/soapsuds.txt", header=T)
head(soapsuds)
attach(soapsuds)

model <- lm(suds ~ soap, x=T)
summary(model)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2.6786     4.2220  -0.634    0.554    
# soap          9.5000     0.7553  12.579 5.64e-05 ***

X <- model$x
t(X) %*% X
#             (Intercept)   soap
# (Intercept)         7.0  38.50
# soap               38.5 218.75

t(X) %*% suds
#             [,1]
# (Intercept)  347
# soap        1975

solve(t(X) %*% X)
#             (Intercept)       soap
# (Intercept)   4.4642857 -0.7857143
# soap         -0.7857143  0.1428571

solve(t(X) %*% X) %*% (t(X) %*% suds)
#                  [,1]
# (Intercept) -2.678571
# soap         9.500000

soap2 <- 2*soap
model <- lm(suds ~ soap + soap2)
summary(model)
# Coefficients: (1 not defined because of singularities)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2.6786     4.2220  -0.634    0.554    
# soap          9.5000     0.7553  12.579 5.64e-05 ***
# soap2             NA         NA      NA       NA    

detach(soapsuds)
