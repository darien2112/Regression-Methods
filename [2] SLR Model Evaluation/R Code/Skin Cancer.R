skincancer <- skincancer 
attach(skincancer)

model <- lm(Mort ~ Lat)

plot(x=Lat, y=Mort,
     xlab = "Latitude (at center of state)", ylab = "Mortality (deaths per 10 million)",
     panel.last = lines(sort(Lat), fitted(model)[order(Lat)]))

summary(model)

confint(model, level = 0.95)

anova(model)
