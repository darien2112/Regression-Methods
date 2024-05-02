X <- c(rep(1, 100), rep(2, 100), rep(3, 100), rep(4, 100))
Y <- 2 + 4*X + rnorm(400, 0, 1)
plot(X, Y, xlab="High school gpa", ylab="College entrance test score",
     panel.last = lines(X, 2+4*X))
Xs <- c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3))
Ys <- Y[c(rep(0, 3), rep(100, 3), rep(200, 3), rep(300, 3)) + sample.int(100, 12)]
model <- lm(Ys ~ Xs)
plot(Xs, Ys, xlab="High school gpa", ylab="College entrance test score",
     panel.last = c(lines(Xs, 2+4*Xs),
                    lines(sort(Xs), fitted(model[order(Xs)]),lty=2))
     sum(residuals(model)^2) # SSE = 8.677833
     sum(residuals(model)^2)/10 # MSE = 0.8677833
     sqrt(sum(residuals(model)^2)/10) # S = 0.9315489
     summary(model) # Residual standard error: 0.9315 on 10 degrees of freedom
     