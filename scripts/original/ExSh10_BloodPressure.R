y <- c(0.9, 2.0, 3.5, 2.6, 4.7, 5.4, 6.1, 8.1, 9.8, 11.6)
x <- 1:10

model1 <- lm(y ~ x) 
model2 <- lm(y ~ poly(x,2)) 
model3 <- lm(y ~ poly(x,3))

anova(model1, model2, model3)

plot(x, y)
abline(model1)
lines(x, predict(model2, x = 1:10), col = 2)
lines(x, predict(model3, x = 1:10), col = 4)

### Blood Pressure ##################################

BloodPressure <- read.csv("BloodPressure.csv")


BloodPressure <- BloodPressure[complete.cases(BloodPressure),]
BloodPressure$Gender
BloodPressure$Gender <- as.factor(ifelse(BloodPressure$Gender == "male", 
                                         yes = "male", no = "female"))

BloodPressure$BMI <- with(BloodPressure,
                          WeightInKg/(HeightInCm/100)^2)

pairs(BloodPressure)

library(PerformanceAnalytics)
chart.Correlation(BloodPressure[,c(1,2,4,5,6,8)], histogram=TRUE, pch=19)


library(corrplot)


?corrplot
corrplot(cor(BloodPressure[,c(1,2,4,5,6)]), type = "upper")
model_syst <- lm(Systolic ~ Age + Gender + WeightInKg + HeightInCm,
             data = BloodPressure)
summary(model_syst)

op <- par(mfrow = c(2,2))
plot(model_syst)
par(op)