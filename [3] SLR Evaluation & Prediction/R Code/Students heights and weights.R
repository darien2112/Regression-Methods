heighweight <- student_height_weight
attach(heighweight)

model <- lm(wt ~ ht)

plot(x = ht, y = wt,
     panel.last = c(lines(sort(ht), fitted(model)[order(ht)]),
                    abline(h = mean(wt))))

mean(wt)

predict(model, newdata = data.frame(ht = 64))

detach(heighweight)
