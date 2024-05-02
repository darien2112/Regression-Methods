heightgpa 
attach(heightgpa)

model <- lm(gpa ~ height)

plot(x = height, y = gpa, 
     xlab = "Height (inches)", ylab = "Grade Point Average",
     panel.last = lines(sort(height), fitted(model)[order(height)]))

summary(model)

anova(model)

detach(heightgpa)
