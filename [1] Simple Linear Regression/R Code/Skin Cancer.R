attach(skincancer)
model <- lm(Mort ~ Lat)
plot(x = Lat, y = Mort, 
     xlab = "Latitude (at center of state)", ylab = "Mortality (deaths per 10 millions)",
     main = "Skin Cancer Mortality versus State Latitude", 
     panel.last = lines(sort(Lat), fitted(model)[order(Lat)]))
detach(skincancer)
     