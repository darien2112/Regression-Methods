peru <- read.table("~/path-to-folder/peru.txt", header=T)
attach(peru)

FracLife <- Years/Age

model.1 <- lm(Systol ~ Age + Years + FracLife + Weight + Height + Chin +
                Forearm + Calf + Pulse)
summary(model.1)
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  146.81907   48.97096   2.998 0.005526 ** 
# Age           -1.12144    0.32741  -3.425 0.001855 ** 
# Years          2.45538    0.81458   3.014 0.005306 ** 
# FracLife    -115.29395   30.16900  -3.822 0.000648 ***
# Weight         1.41393    0.43097   3.281 0.002697 ** 
# Height        -0.03464    0.03686  -0.940 0.355194    
# Chin          -0.94369    0.74097  -1.274 0.212923    
# Forearm       -1.17085    1.19329  -0.981 0.334612    
# Calf          -0.15867    0.53716  -0.295 0.769810    
# Pulse          0.11455    0.17043   0.672 0.506818    

anova(model.1)
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
# Age        1    0.22    0.22  0.0030  0.956852    
# Years      1   82.55   82.55  1.1019  0.302514    
# FracLife   1 3112.41 3112.41 41.5449 4.728e-07 ***
# Weight     1  706.54  706.54  9.4311  0.004603 ** 
# Height     1    1.68    1.68  0.0224  0.882117    
# Chin       1  297.68  297.68  3.9735  0.055704 .  
# Forearm    1  113.91  113.91  1.5205  0.227440    
# Calf       1   10.01   10.01  0.1336  0.717420    
# Pulse      1   33.84   33.84  0.4518  0.506818    
# Residuals 29 2172.58   74.92                      

model.2 <- lm(Systol ~ Age + Years + FracLife + Weight)
summary(model.2)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  116.8354    21.9797   5.316 6.69e-06 ***
# Age           -0.9507     0.3164  -3.004 0.004971 ** 
# Years          2.3393     0.7714   3.032 0.004621 ** 
# FracLife    -108.0728    28.3302  -3.815 0.000549 ***
# Weight         0.8324     0.2754   3.022 0.004742 ** 

anova(model.2)
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
# Age        1    0.22    0.22  0.0029  0.957480    
# Years      1   82.55   82.55  1.0673  0.308840    
# FracLife   1 3112.41 3112.41 40.2409 3.094e-07 ***
# Weight     1  706.54  706.54  9.1350  0.004742 ** 
# Residuals 34 2629.71   77.34                      

(2629.71-2172.58)/(34-29) / (2172.58/29) # F = 1.220371
pf(1.220371, 5, 29, lower.tail=F) # p-value = 0.3247213

anova(model.2, model.1)
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     34 2629.7                           
# 2     29 2172.6  5    457.12 1.2204 0.3247

detach(peru)
