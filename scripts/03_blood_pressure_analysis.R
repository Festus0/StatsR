# StatsR: Blood Pressure EDA + Linear Model

file <- "data/raw/BloodPressure.csv"
if (!file.exists(file)) {
  stop("BloodPressure.csv not found in data/raw/")
}

bp <- read.csv(file)
bp <- na.omit(bp)

# Create BMI
bp$BMI <- bp$WeightInKg / (bp$HeightInCm/100)^2

print(summary(bp))

# Correlation matrix
num <- bp[, c("Systolic","Diastolic","Age","WeightInKg","HeightInCm","BMI")]
print(cor(num))

# Linear Model
model <- lm(Systolic ~ Age + WeightInKg + HeightInCm, data = bp)
print(summary(model))

png("assets/bp_diagnostics.png", width=1200, height=800)
par(mfrow=c(2,2))
plot(model)
dev.off()
