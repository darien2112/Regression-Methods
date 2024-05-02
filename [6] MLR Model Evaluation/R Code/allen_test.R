allentest <- read.table("~/path-to-folder/allentest.txt", header=T)
attach(allentest)

sum((ACL-mean(ACL))^2) # SSTO = 43.04957

model.1 <- lm(ACL ~ Vocab)
anova(model.1)
# Analysis of Variance Table
# Response: ACL
#           Df Sum Sq Mean Sq F value  Pr(>F)  
# Vocab      1  2.691 2.69060  4.4667 0.03829 *
# Residuals 67 40.359 0.60237                  

model.13 <- lm(ACL ~ Vocab + SDMT)
anova(model.13) # Sequential (type I) SS
#           Df  Sum Sq Mean Sq F value   Pr(>F)    
# Vocab      1  2.6906  2.6906  5.6786  0.02006 *  
# SDMT       1  9.0872  9.0872 19.1789 4.35e-05 ***
# Residuals 66 31.2717  0.4738                     
# Calculate by hand: SSR(Vocab, SDMT) = 2.6906 + 9.0872 = 11.7778

model.3 <- lm(ACL ~ SDMT)
anova(model.3)
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# SDMT       1  11.68 11.6799  24.946 4.468e-06 ***
# Residuals 67  31.37  0.4682                      

model.31 <- lm(ACL ~ SDMT + Vocab)
anova(model.31) # Sequential (type I) SS
#           Df  Sum Sq Mean Sq F value   Pr(>F)    
# SDMT       1 11.6799 11.6799 24.6508 5.12e-06 ***
# Vocab      1  0.0979  0.0979  0.2067   0.6508    
# Residuals 66 31.2717  0.4738                     
# Calculate by hand: SSR(Vocab, SDMT) = 11.6799 + 0.0979 = 11.7778

model.312 <- lm(ACL ~ SDMT + Vocab + Abstract)
anova(model.312) # Sequential (type I) SS
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
# SDMT       1 11.6799 11.6799 24.6902 5.173e-06 ***
# Vocab      1  0.0979  0.0979  0.2070    0.6506    
# Abstract   1  0.5230  0.5230  1.1056    0.2969    
# Residuals 65 30.7487  0.4731                      
# Calculate by hand: SSR(Vocab, Abstract | SDMT) = 0.0979 + 0.5230 = 0.6209

detach(allentest)