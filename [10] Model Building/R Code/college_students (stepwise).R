physical <- read.table("~/path-to-data/Physical.txt", header=T)
attach(physical)

gender <- ifelse(Sex=="Female",1,0)

model.0 <- lm(Height ~ 1)
add1(model.0, ~ LeftArm + LeftFoot + LeftHand + HeadCirc + nose + gender, test="F")
# Model:
# Height ~ 1
#          Df Sum of Sq     RSS    AIC  F value    Pr(>F)    
#                 1054.75 164.46                       
# LeftArm   1    590.21  464.53 121.35  67.3396 5.252e-11 ***
# LeftFoot  1    707.42  347.33 105.36 107.9484 2.172e-14 ***
# LeftHand  1    143.59  911.15 158.41   8.3525  0.005570 ** 
# HeadCirc  1    189.24  865.51 155.58  11.5880  0.001272 ** 
# nose      1     85.25  969.49 161.82   4.6605  0.035412 *  
# gender    1    533.24  521.51 127.72  54.1923 1.181e-09 ***

model.2 <- lm(Height ~ LeftFoot)
add1(model.2, ~ . + LeftArm + LeftHand + HeadCirc + nose + gender, test="F")
# Model:
# Height ~ LeftFoot
#         Df Sum of Sq    RSS     AIC F value    Pr(>F)    
#                 347.33 105.361                      
# LeftArm   1   107.143 240.18  87.074 23.1967 1.305e-05 ***
# LeftHand  1    15.359 331.97 104.874  2.4059    0.1269    
# HeadCirc  1     2.313 345.01 106.994  0.3486    0.5575    
# nose      1     1.449 345.88 107.131  0.2178    0.6427    
# gender    1    15.973 331.35 104.772  2.5066    0.1194    

model.12 <- lm(Height ~ LeftArm + LeftFoot)
drop1(model.12, ~ ., test="F")
# Model:
# Height ~ LeftArm + LeftFoot
#          Df Sum of Sq    RSS     AIC F value    Pr(>F)    
#                 240.18  87.074                      
# LeftArm   1    107.14 347.33 105.361  23.197 1.305e-05 ***
# LeftFoot  1    224.35 464.53 121.353  48.572 5.538e-09 ***

add1(model.12, ~ . + LeftHand + HeadCirc + nose + gender, test="F")
# Model:
# Height ~ LeftArm + LeftFoot
#          Df Sum of Sq    RSS    AIC F value Pr(>F)
#                 240.18 87.074               
# LeftHand  1    3.7854 236.40 88.200  0.8167 0.3704
# HeadCirc  1    1.4016 238.78 88.752  0.2994 0.5867
# nose      1    0.4463 239.74 88.971  0.0950 0.7592
# gender    1    3.7530 236.43 88.207  0.8096 0.3725

subset <- regsubsets(Height ~ LeftArm + LeftFoot + LeftHand + HeadCirc + nose + gender,
                     method="backward", data=physical)

subset <- regsubsets(Height ~ LeftArm + LeftFoot + LeftHand + HeadCirc + nose + gender,
                     method="forward", data=physical)

detach(physical)