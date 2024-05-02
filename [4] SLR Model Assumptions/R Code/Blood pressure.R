head(bloodpress)
attach(bloodpress)

model.1 <- lm(BP ~ Age)
summary(model.1)

plot(x = Age, y = BP,
     xlab = "Age (years)", ylab = "Diastolic blood pressure (mm Hg)",
     panel.last = lines(sort(Age), fitted(model.1)[order(Age)]))

model.2 <- lm(BP ~ Weight)
summary(model.2)

plot(x = Weight, y = BP,
     xlab = "Weight (pounds)", ylab = "Diastolic blood pressure (mm Hg)",
     panel.last = lines(sort(Weight), fitted(model.2)[order(Weight)]))

model.3 <- lm(BP ~ Dur)
summary(model.3)

plot(x = Dur, y = BP,
     xlab = "Duration of hypertension (years)",
     ylab = "Diastolic blood pressure (mmHg)",
     panel.last = lines(sort(Dur), fitted(model.3)[order(Dur)]))

plot(x = Weight, y = residuals(model.1),
     xlab = "Weight (pounds)", ylab = "Residuals from model with Age",
     panel.last = abline(h = 0, lty = 2))

model.12 <- lm(BP ~ Age + Weight)

plot(x = Dur, y = residuals(model.12),
     xlab = "Duration of hypertension (years)",
     ylab = "Residuals from model with Age and Weight",
     panel.last = abline(h = 0, lty = 2))

detach(bloodpress)
