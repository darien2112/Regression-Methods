heightgpa <- read.table("~/path-to-folder/heightgpa.txt", header=T)
attach(heightgpa)
model <- lm(gpa ~ height)
summary(model) # Multiple R-squared:  0.002835
cor(gpa, height) # correlation = -0.05324126
detach(heightgpa)