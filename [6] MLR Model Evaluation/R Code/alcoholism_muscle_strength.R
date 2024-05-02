alcoholarm <- read.table("~/path-to-folder/alcoholarm.txt", header=T)
attach(alcoholarm)

model <- lm(strength ~ alcohol)
sum((strength-mean(strength))^2) # SSE_R = 1224.315
sum(residuals(model)^2) # SSE_F = 720.2749
((1224.315-720.2749)/1) / (720.2749/48) # F = 33.58985
summary(model) # F-statistic: 33.59 on 1 and 48 DF,  p-value: 5.136e-07
anova(model)
# Analysis of Variance Table
# Response: strength
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# alcohol    1 504.04  504.04   33.59 5.136e-07 ***
# Residuals 48 720.27   15.01                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

detach(alcoholarm)