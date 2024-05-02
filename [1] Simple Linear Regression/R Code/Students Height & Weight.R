heightweight <- student_height_weight
attach(heightweight)

model <- lm(wt ~ ht)
summary(model)

plot(x = ht, y = wt,
     ylim = c(110, 210),
     xlab = "height", ylab = "weight", 
     panel.last = c(lines(sort(ht), fitted(model[order(ht)]),
                    lines(ht, -331.2+7.1*ht, lty = 2))))

# SSE
sum(residuals(model)^2)      

# Predict
predict(model, newdata = data.frame(ht = c(66, 67)))

detach(heightweight)
