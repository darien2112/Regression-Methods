skincancer
attach(skincancer)

model <- lm(Mort ~ Lat)

plot(x=Lat, y=Mort,
     xlab="Latitude (at center of state)", ylab="Mortality (deaths per 10 million)",
     main="Skin Cancer Mortality versus State Latitude",
     panel.last = lines(sort(Lat), fitted(model)[order(Lat)]))

predict(model, interval = "confidence", se.fit = T,
        newdata = data.frame(Lat = c(40, 28)))

mean(Lat)

predict(model, interval = "prediction",
        newdata = data.frame(Lat = 40))

plot(x = Lat, y = Mort,
     xlab = "Latitude (at center of state)", ylab = "Mortality (deaths per 10 million)",
     main = "Skin Cancer Mortality vs. State Latitude (CI & PI Plotted)",
     ylim = c(60, 260),
     panel.last = c(lines(sort(Lat), fitted(model)[order(Lat)]),
                    lines(sort(Lat),
                          predict(model,
                                  interval = "confidence")[order(Lat), 2], col = "green"),
                    lines(sort(Lat),
                          predict(model,
                                  interval = "confidence")[order(Lat), 3], col = "green"),
                    lines(sort(Lat),
                          predict(model,
                                  interval = "prediction")[order(Lat), 2], col = "purple"),
                    lines(sort(Lat),
                          predict(model,
                                  interval = "prediction")[order(Lat), 3], col = "purple")))

detach(skincancer)
