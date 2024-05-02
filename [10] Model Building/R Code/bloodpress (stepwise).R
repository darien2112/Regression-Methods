bloodpress <- read.table("~/path-to-data/bloodpress.txt", header=T)
attach(bloodpress)

pairs(bloodpress[,c(2:5)])
pairs(bloodpress[,c(2,6:8)])

model.0 <- lm(BP  ~ 1)
add1(model.0, ~ Age + Weight + BSA + Dur + Pulse + Stress, test="F")
# Model:
# BP ~ 1
#        Df Sum of Sq    RSS    AIC  F value    Pr(>F)    
#               560.00 68.644                       
# Age     1    243.27 316.73 59.247  13.8248 0.0015737 ** 
# Weight  1    505.47  54.53 24.060 166.8591 1.528e-10 ***
# BSA     1    419.86 140.14 42.938  53.9270 8.114e-07 ***
# Dur     1     48.02 511.98 68.851   1.6883 0.2102216    
# Pulse   1    291.44 268.56 55.946  19.5342 0.0003307 ***
# Stress  1     15.04 544.96 70.099   0.4969 0.4898895    

model.2 <- lm(BP ~ Weight)
add1(model.2, ~ . + Age + BSA + Dur + Pulse + Stress, test="F")
# Model:
# BP ~ Weight
#        Df Sum of Sq    RSS     AIC  F value    Pr(>F)    
#               54.528  24.060                       
# Age     1    49.704  4.824 -22.443 175.1622 2.218e-10 ***
# BSA     1     2.814 51.714  25.000   0.9251   0.34962    
# Dur     1     6.095 48.433  23.689   2.1393   0.16181    
# Pulse   1     8.940 45.588  22.478   3.3338   0.08549 .  
# Stress  1     9.660 44.868  22.160   3.6601   0.07273 .  

model.12 <- lm(BP ~ Age + Weight)
drop1(model.12, ~ ., test="F")
# Model:
# BP ~ Age + Weight
#        Df Sum of Sq    RSS     AIC F value    Pr(>F)    
#                 4.82 -22.443                      
# Age     1    49.704  54.53  24.060  175.16 2.218e-10 ***
# Weight  1   311.910 316.73  59.247 1099.20 < 2.2e-16 ***

add1(model.12, ~ . + BSA + Dur + Pulse + Stress, test="F")
# Model:
# BP ~ Age + Weight
#        Df Sum of Sq    RSS     AIC F value   Pr(>F)   
#               4.8239 -22.443                    
# BSA     1   1.76778 3.0561 -29.572  9.2550 0.007764 **
# Dur     1   0.17835 4.6456 -21.196  0.6143 0.444639   
# Pulse   1   0.49557 4.3284 -22.611  1.8319 0.194719   
# Stress  1   0.16286 4.6611 -21.130  0.5591 0.465486   

model.123 <- lm(BP ~ Age + Weight + BSA)
drop1(model.123, ~ ., test="F")
# Model:
# BP ~ Age + Weight + BSA
#        Df Sum of Sq    RSS     AIC F value    Pr(>F)    
#                3.056 -29.572                      
# Age     1    48.658 51.714  25.000 254.740 3.002e-11 ***
# Weight  1    65.303 68.359  30.581 341.886 3.198e-12 ***
# BSA     1     1.768  4.824 -22.443   9.255  0.007764 ** 

add1(model.123, ~ . + Dur + Pulse + Stress, test="F")
# Model:
# BP ~ Age + Weight + BSA
#        Df Sum of Sq    RSS     AIC F value Pr(>F)
#               3.0561 -29.572               
# Dur     1   0.33510 2.7210 -29.894  1.8473 0.1942
# Pulse   1   0.04111 3.0150 -27.842  0.2045 0.6576
# Stress  1   0.21774 2.8384 -29.050  1.1507 0.3004

detach(bloodpress)