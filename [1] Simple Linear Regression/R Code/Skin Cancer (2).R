skincancer <- read.table("~/path-to-folder/skincancer.txt", header=T)
attach(skincancer)
model <- lm(Mort ~ Lat)
summary(model) # Multiple R-squared:  0.6798
cor(Mort, Lat) # correlation = -0.8245178
detach(skincancer)
