iqsize <- read.table("~/path-to-folder/iqsize.txt", header=T)
head(iqsize)
attach(iqsize)

pairs(cbind(PIQ, Brain, Height, Weight))

model <- lm(PIQ ~ Brain + Height + Weight)
summary(model)

anova(model)

library(car)
Anova(model, type="III") # Adjusted (type III) SS

detach(iqsize)
