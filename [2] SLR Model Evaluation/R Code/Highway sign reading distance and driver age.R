signdist
attach(signdist)

model <- lm(Distance ~ Age)

plot(x = Age, y = Distance, 
     xlab = "Age", ylab = "Distance", 
     panel.last = lines(sort(Age), fitted(model)[order(Age)]))

summary(model)

confint(model, parm = "Age", level = 0.95)

confint(model, parm = "Age", level = 0.99)

detach(signdist)
