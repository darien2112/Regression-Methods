cement <- read.table("~/path-to-data/cement.txt", header=T)
attach(cement)

pairs(cement)

model.0 <- lm(y ~ 1)
add1(model.0, ~ x1 + x2 + x3 + x4, test="F")
# Model:
# y ~ 1
#        Df Sum of Sq     RSS    AIC F value    Pr(>F)    
#               2715.76 71.444                      
# x1      1   1450.08 1265.69 63.519 12.6025 0.0045520 ** 
# x2      1   1809.43  906.34 59.178 21.9606 0.0006648 ***
# x3      1    776.36 1939.40 69.067  4.4034 0.0597623 .  
# x4      1   1831.90  883.87 58.852 22.7985 0.0005762 ***

model.4 <- lm(y ~ x4)
add1(model.4, ~ . + x1 + x2 + x3, test="F")
# Model:
# y ~ x4
#        Df Sum of Sq    RSS    AIC  F value    Pr(>F)    
#               883.87 58.852                       
# x1      1    809.10  74.76 28.742 108.2239 1.105e-06 ***
# x2      1     14.99 868.88 60.629   0.1725    0.6867    
# x3      1    708.13 175.74 39.853  40.2946 8.375e-05 ***

model.14 <- lm(y ~ x1 + x4)
drop1(model.14, ~ ., test="F")
# Model:
# y ~ x1 + x4
#        Df Sum of Sq     RSS    AIC F value    Pr(>F)    
#                 74.76 28.742                      
# x1      1     809.1  883.87 58.852  108.22 1.105e-06 ***
# x4      1    1190.9 1265.69 63.519  159.30 1.815e-07 ***

add1(model.14, ~ . + x2 + x3, test="F")
# Model:
# y ~ x1 + x4
#        Df Sum of Sq    RSS    AIC F value  Pr(>F)  
#               74.762 28.742                  
# x2      1    26.789 47.973 24.974  5.0259 0.05169 .
# x3      1    23.926 50.836 25.728  4.2358 0.06969 .

model.124 <- lm(y ~ x1 + x2 + x4)
drop1(model.124, ~ ., test="F")
# Model:
# y ~ x4 + x1 + x2
#        Df Sum of Sq    RSS    AIC  F value    Pr(>F)    
#                47.97 24.974                       
# x1      1    820.91 868.88 60.629 154.0076 5.781e-07 ***
# x2      1     26.79  74.76 28.742   5.0259   0.05169 .  
# x4      1      9.93  57.90 25.420   1.8633   0.20540    

model.12 <- lm(y ~ x1 + x2)
add1(model.12, ~ . + x3 + x4, test="F")
# Model:
# y ~ x1 + x2
#        Df Sum of Sq    RSS    AIC F value Pr(>F)
#               57.904 25.420               
# x3      1    9.7939 48.111 25.011  1.8321 0.2089
# x4      1    9.9318 47.973 24.974  1.8633 0.2054

detach(cement)