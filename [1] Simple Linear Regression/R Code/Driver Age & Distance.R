signdist <- read.table("~/path-to-folder/signdist.txt", header=T)
attach(signdist)
model <- lm(Distance ~ Age)
summary(model) # Multiple R-squared:  0.642
cor(Distance, Age) # correlation = -0.8012447
detach(signdist)