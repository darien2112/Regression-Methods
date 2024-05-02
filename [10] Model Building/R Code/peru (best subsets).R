peru <- read.table("~/path-to-data/peru.txt", header=T)
attach(peru)

fraclife <- Years/Age

n <- length(Systol) # 39

subset <- regsubsets(Systol ~ Age + Years + fraclife + Weight + Height + Chin +
                       Forearm + Pulse,
                     method="exhaustive", nbest=2, data=peru)
cbind(summary(subset)$outmat, round(summary(subset)$rsq, 3),
      round(summary(subset)$adjr2, 3), round(summary(subset)$cp, 1),
      round(sqrt(summary(subset)$rss/(n-c(rep(1:7,rep(2,7)),8)-1)), 4))
#          Age Years fraclife Weight Height Chin Forearm Pulse                                 
# 1  ( 1 ) " " " "   " "      "*"    " "    " "  " "     " "   "0.272" "0.252" "30.5" "11.3376"
# 1  ( 2 ) " " " "   "*"      " "    " "    " "  " "     " "   "0.076" "0.051" "48.1" "12.7697"
# 2  ( 1 ) " " " "   "*"      "*"    " "    " "  " "     " "   "0.473" "0.444" "14.4" "9.7772" 
# 2  ( 2 ) " " "*"   " "      "*"    " "    " "  " "     " "   "0.421" "0.389" "19.1" "10.2512"
# 3  ( 1 ) " " " "   "*"      "*"    " "    "*"  " "     " "   "0.503" "0.461" "13.7" "9.6273" 
# 3  ( 2 ) " " "*"   "*"      "*"    " "    " "  " "     " "   "0.49"  "0.447" "14.8" "9.7509" 
# 4  ( 1 ) "*" "*"   "*"      "*"    " "    " "  " "     " "   "0.597" "0.55"  "7.2"  "8.7946" 
# 4  ( 2 ) "*" "*"   "*"      " "    "*"    " "  " "     " "   "0.525" "0.469" "13.7" "9.5502" 
# 5  ( 1 ) "*" "*"   "*"      "*"    " "    "*"  " "     " "   "0.639" "0.584" "5.5"  "8.4571" 
# 5  ( 2 ) "*" "*"   "*"      "*"    " "    " "  "*"     " "   "0.631" "0.576" "6.1"  "8.5417" 
# 6  ( 1 ) "*" "*"   "*"      "*"    " "    "*"  "*"     " "   "0.649" "0.583" "6.6"  "8.4663" 
# 6  ( 2 ) "*" "*"   "*"      "*"    "*"    "*"  " "     " "   "0.643" "0.576" "7.1"  "8.5337" 
# 7  ( 1 ) "*" "*"   "*"      "*"    "*"    "*"  "*"     " "   "0.661" "0.584" "7.5"  "8.4556" 
# 7  ( 2 ) "*" "*"   "*"      "*"    " "    "*"  "*"     "*"   "0.655" "0.577" "8"    "8.522"  
# 8  ( 1 ) "*" "*"   "*"      "*"    "*"    "*"  "*"     "*"   "0.666" "0.577" "9"    "8.5228" 

model.5 <- lm(Systol ~ Age + Years + fraclife + Weight + Chin)
summary(model.5)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  109.3590    21.4843   5.090 1.41e-05 ***
# Age           -1.0120     0.3059  -3.308 0.002277 ** 
# Years          2.4067     0.7426   3.241 0.002723 ** 
# fraclife    -110.8112    27.2795  -4.062 0.000282 ***
# Weight         1.0976     0.2980   3.683 0.000819 ***
# Chin          -1.1918     0.6140  -1.941 0.060830 .  
# ---
# Residual standard error: 8.457 on 33 degrees of freedom
# Multiple R-squared:  0.6386,  Adjusted R-squared:  0.5839 
# F-statistic: 11.66 on 5 and 33 DF,  p-value: 1.531e-06

k <- 5
n*log(sum(residuals(model.5)^2))-n*log(n)+2*(k+1) # AIC = 172.0151
n*log(sum(residuals(model.5)^2))-n*log(n)+log(n)*(k+1) # BIC = 181.9965

model.4 <- lm(Systol ~ Age + Years + fraclife + Weight)
summary(model.4)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  116.8354    21.9797   5.316 6.69e-06 ***
# Age           -0.9507     0.3164  -3.004 0.004971 ** 
# Years          2.3393     0.7714   3.032 0.004621 ** 
# fraclife    -108.0728    28.3302  -3.815 0.000549 ***
# Weight         0.8324     0.2754   3.022 0.004742 ** 
# ---
# Residual standard error: 8.795 on 34 degrees of freedom
# Multiple R-squared:  0.5974,  Adjusted R-squared:   0.55 
# F-statistic: 12.61 on 4 and 34 DF,  p-value: 2.142e-06

k <- 4
n*log(sum(residuals(model.4)^2))-n*log(n)+2*(k+1) # AIC = 174.2316
n*log(sum(residuals(model.4)^2))-n*log(n)+log(n)*(k+1) # BIC = 182.5494

library(MASS)
subset.aic <- stepAIC(lm(Systol ~ Age + Years + fraclife + Weight + Height +
                           Chin + Forearm + Pulse), direction="both", k=2)
# Step:  AIC=172.02
# Systol ~ Age + Years + fraclife + Weight + Chin

subset.bic <- stepAIC(lm(Systol ~ Age + Years + fraclife + Weight + Height +
                           Chin + Forearm + Pulse), direction="both", k=log(n))
# Step:  AIC=182
# Systol ~ Age + Years + fraclife + Weight + Chin

detach(peru)