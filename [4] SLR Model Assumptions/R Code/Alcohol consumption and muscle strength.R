alcoholarm
attach(alcoholarm)

model <- lm(strength ~ alcohol)

summary(model)

plot(x = alcohol, y = strength,
     xlab = "Lifetime consumption of alcohol", ylab = "Deltoid muscle strength",
     panel.last = lines(sort(alcohol), fitted(model)[order(alcohol)]))

plot(x = fitted(model), y = residuals(model),
     xlab = "Fitted values", ylab = "Residuals",
     panel.last = abline(h = 0, lty = 2))

plot(x = alcohol, y = residuals(model),
     xlab = "Lifetime consumption of alcohol", ylab = "Residuals",
     panel.last = abline(h = 0, lty = 2))

detach(alcoholarm)
