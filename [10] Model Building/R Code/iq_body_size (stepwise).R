iqsize <- read.table("~/path-to-data/iqsize.txt", header=T)
attach(iqsize)

pairs(iqsize)

model.0 <- lm(PIQ ~ 1)
add1(model.0, ~ Brain + Height + Weight, test="F")
# Model:
# PIQ ~ 1
#        Df Sum of Sq   RSS    AIC F value  Pr(>F)  
#               18895 237.94                  
# Brain   1   2697.09 16198 234.09  5.9945 0.01935 *
# Height  1    163.97 18731 239.61  0.3151 0.57802  
# Weight  1      0.12 18894 239.94  0.0002 0.98806  

model.1 <- lm(PIQ ~ Brain)
add1(model.1, ~ . + Height + Weight, test="F")
# Model:
# PIQ ~ Brain
#        Df Sum of Sq   RSS    AIC F value   Pr(>F)   
#               16198 234.09                    
# Height  1   2875.65 13322 228.66  7.5551 0.009399 **
# Weight  1    940.94 15256 233.82  2.1586 0.150705   

model.12 <- lm(PIQ ~ Brain + Height)
drop1(model.12, ~ ., test="F")
# Model:
# PIQ ~ Brain + Height
#        Df Sum of Sq   RSS    AIC F value    Pr(>F)    
#               13322 228.66                      
# Brain   1    5408.8 18731 239.61 14.2103 0.0006045 ***
# Height  1    2875.6 16198 234.09  7.5551 0.0093991 ** 

add1(model.12, ~ . + Weight, test="F")
# Model:
# PIQ ~ Brain + Height
#        Df Sum of Sq   RSS    AIC F value Pr(>F)
#               13322 228.66               
# Weight  1 0.0031633 13322 230.66       0 0.9977

detach(iqsize)