# StatsR: R Basics + Functions
set.seed(1)

x <- rep(1:4, each = 2)
y <- rep(1:4, times = 2)
cat("Correlation:", cor(x, y), "\n")

# Fibonacci
fibonacci <- function(n = 10) {
  fib <- numeric(n)
  fib[1:2] <- 1
  for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
  return(fib)
}

print(fibonacci(10))

# BMI Function
bmi <- function(weight, height_cm) {
  weight / (height_cm/100)^2
}

print(bmi(70, 175))
