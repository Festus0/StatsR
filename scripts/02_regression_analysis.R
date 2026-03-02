# StatsR: Regression & Model Comparison

y <- c(0.9,2.0,3.5,2.6,4.7,5.4,6.1,8.1,9.8,11.6)
x <- 1:10

m1 <- lm(y ~ x)
m2 <- lm(y ~ poly(x,2))
m3 <- lm(y ~ poly(x,3))

print(anova(m1, m2, m3))

png("assets/regression_plot.png", width=1000, height=700)
plot(x, y, pch=19, main="Regression Comparison")
lines(x, predict(m1), col="blue", lwd=2)
lines(x, predict(m2), col="red", lwd=2)
lines(x, predict(m3), col="green", lwd=2)
legend("topleft", legend=c("Linear","Quadratic","Cubic"),
       col=c("blue","red","green"), lty=1)
dev.off()
