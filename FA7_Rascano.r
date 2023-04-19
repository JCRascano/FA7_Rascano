#FA7
install.packages("rmarkdown")
# FOR NUMBER 1

# 1A
# Range of the Memory Stick Lengths
MemStick_MaxLen <- 12
MemStick_MinLen <- 2


# Probability that a memory stick will be scrapped
Scrapped_Prob <- 1 - punif(10, MemStick_MinLen, MemStick_MaxLen)
cat("Hence, we have shown that the chances where the memory stick will be scrapped is ", Scrapped_Prob, "\n")

# 1B
# Range of the Memory Stick Lengths
MinLen <- 2
MaxLen <- 12

# 50 simulations of memory stick lengths needed
Stick_Len <- runif(50, MinLen, MaxLen)

# Histogram of the simulated values
hist(Stick_Len, main = "Simulated Memory Stick Lengths", xlab = "Length", ylab = "Frequency")

# Mean and variance of simulations
Stick_Len_Mean <- mean(Stick_Len)
cat("The Variance of the Simulated Memory Stick Lengths is:", Stick_Len_Mean)

Stick_Len_Variance <- var(Stick_Len)
cat("The Variance of the Simulated Memory Stick Lengths is:", Stick_Len_Variance)
#End of Number 1

# FOR NUMBER 2

# 2A
# Given function
b <- 0.15
Prob_Density_2AFunction <- function(x) {0.025 * x + b}

# Integral of the function over the interval from 2 to 6
Integ_2AResult <- integrate(Prob_Density_2AFunction, 2, 6)

# Extraction of the numerical value of the result using the class $value of integrate
Gen_Prob_Density <- Integ_2AResult$value
Gen_Prob_Density



# 2B
# Given function with the calculated value of b = 0.15
Prob_Density_2BFunction <- function(x) {0.025 * x + 0.15}

# Integral of the pdf over the interval from 3 to 6
Integ_2BResult <- integrate(Prob_Density_2BFunction, 3, 6)

# Extraction of the numerical value of the result using the class $value of integrate
Exceed_Probon3 <- Integ_2BResult$value
Exceed_Probon3



# 2C
# Given function with the calculated value of b = 0.15
Prob_Density_2CFunction <- function(x) {0.025*x + 0.15}

# Specify the integration limits for the integral
Lower_Val <- 2
Upper_Val <- 6

# Integrand for E(X)
integrand <- function(x) {x * Prob_Density_2CFunction(x)}

# Using the integrate function we will get the expected value
# Extraction of the numerical value of the result using the class $value of integrate
EX <- integrate(integrand, Lower_Val, Upper_Val)$value
EX

#End of Number 2

# NUMBER (3)

# Range of the Uniform Distribution
Unif_Dis_MaxRange <- pi
Unif_Dis_MinRange <- -pi

# Calculation of P(X ≤ 0) and P(X ≤ 𝜋/2)

# FOR P(X ≤ 0)
Prob_LessEq0 <- punif(0, Unif_Dis_MinRange, Unif_Dis_MaxRange)
Prob_LessEq0

# FOR P(X ≤ 𝜋/2)
Prob_LessEq_PiOver2 <- punif(pi/2, Unif_Dis_MinRange, Unif_Dis_MaxRange)
Prob_LessEq_PiOver2 
#End of Number 3


