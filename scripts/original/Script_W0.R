### Script Week 0 #####

# whatever comes after the hashtag is a comment

x <- rep(1:4, each = 2)  #repeat each number twice
y <- rep(1:4, times = 2) #repeat the sequence twice

cor(x,y)

#install.packages("MASS")

### Exercise: create a vector whose elements are the cube of x ####

x_cube <- numeric(length(x))
for(i in 1:length(x)){
  x_cube[i] <- x[i]^3
}


x3 <- x^3

x_cube
x3

### Check if the number a is odd or even ####

a <- c(103,34,5)
if(a %% 2 == 0){
  print("Even")
}else{print("Odd")}

### Check if each number of a vector is odd or even ####

b <- sample.int(n = 100, size = 10)

size_b <- length(b)
for(i in 1:size_b){
  if(b[i] %% 2 == 0){
    print("Even")
  }else{print("Odd")}
}


b_cond <- ifelse(test = b %% 2 == 0,
                 yes = "Even",
                 no = "Odd")

#b_cond <- ifelse(b[i] %% 2 == 0, "Even", "Odd")


data.frame(b, b_cond)


### Fibonacci sequences ##############

fibonacci <- function(n = 20){
  if(n < 3) stop("Please provide n > 2")
  fib <- numeric(n) #numeric create an empty vector (vector with NA values) of length n
  fib[c(1,2)] <- 1
  for(i in 3:n){
    fib[i] <- fib[i-1] + fib[i-2]
  }
  return(fib)
}

fibonacci()
fibonacci(3)
fibonacci(-1)  ###returns an error


### Rugby scores ############

##Which are the valid scores?
#https://www.theguardian.com/science/2015/sep/14/alex-bellos-did-you-solve-it-rugby-points-puzzle
#Anyway, the important part here is not the maths behind, but the correct usage of "if" with logical conditions.

rugby_valid <- function(score = c(1,2)){
  if(length(score) != 2 | class(score) != "numeric" | sum(score >= 0) != 2) stop("Please provide a vector with two positive numbers!")
  condition_rugby <- score %in% c(1,2,4)
  output <- ifelse(sum(condition_rugby) == 0, 
                   "valid score!",
                   "this is not a valid score. Try again!")
  cat(paste0(score[1], "-", score[2],": ", output))
}

rugby_valid()
rugby_valid(c(5,7))
rugby_valid(c("A",6)) ###

condition_rugby  ### please note that the objects created within a function are not saved in the global environment

### Produce a vector containing all the whole numbers whose square root is less than 7.5. ###

x <- 1:100
x[sqrt(x) < 7.5] ###x[condition] in R is interpred as x[condition == TRUE]

### BloodPressure BMIclass ####

# First, import the dataset "BloodPressure" in your environment.
# Second, create the vector BMI using height and weight in the right unit of measurement.
BloodPressure$BMI <- with(BloodPressure, WeightInKg/(HeightInCm/100)^2)

?with  ### the command with is similar to "attach": evaluate the expression within the data "BloodPressure".

for(i in 1:length(BloodPressure$BMI)){
  BloodPressure$BMIclass[i] <- if(BloodPressure$BMI[i] < 18.5){
    "Underweight"
  }else if(BloodPressure$BMI[i] >= 18.5 & BloodPressure$BMI[i] < 25){
    "Normal"
  }else if(BloodPressure$BMI[i] >= 25 & BloodPressure$BMI[i] < 30){
    "Overweight"
  }else if(BloodPressure$BMI[i] >= 30){
    "Obese"
  }
}


## or, much better ...

?cut

BloodPressure$BMIclass2 <- cut(BloodPressure$BMI, 
                              breaks = c(min(BloodPressure$BMI), 18.5, 25 ,30, max(BloodPressure$BMI)),
                              labels = c("Underweight", "Normal", "Overweight", "Obese"),
                              include.lowest = TRUE, 
                              right = FALSE)


all.equal(as.factor(BloodPressure$BMIclass), BloodPressure$BMIclass2)  ### why do the two vector differ?

### Homework: give a look to the commands "factor" and "ordered". #####