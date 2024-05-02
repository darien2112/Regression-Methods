iqsize <- read.table("~/path-to-folder/iqsize.txt", header=T)
head(iqsize, 5)
attach(iqsize)

model <- lm(PIQ ~ Brain + Height)

plot(x=fitted(model), y=residuals(model),
     xlab="Fitted values", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

plot(x=Brain, y=residuals(model),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

plot(x=Height, y=residuals(model),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

hist(residuals(model), main="")

qqnorm(residuals(model), main="", datax=TRUE)
qqline(residuals(model), datax=TRUE)

plot(x=Weight, y=residuals(model),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

library(nortest)
ad.test(residuals(model)) # A = 0.2621, p-value = 0.6857
shapiro.test(residuals(model)) # W = 0.976, p-value = 0.5764
lillie.test(residuals(model)) # D = 0.097, p-value = 0.4897

detach(iqsize)