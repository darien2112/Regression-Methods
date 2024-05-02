bldgstories <- read.table("~/path-to-folder/bldgstories.txt", header=T)
attach(bldgstories)
model <- lm(HGHT ~ STORIES)
summary(model) # Multiple R-squared:  0.9036
cor(HGHT, STORIES) # correlation = 0.9505549
detach(bldgstories)
