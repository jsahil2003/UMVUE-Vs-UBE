# Generate uniform samples and calculate variances of UMVUE and UBE estimators

range_of_n <- c(5,10,50,100,250,500,750,1000,2500,5000,10000)  # Different sample sizes
theta = 4  # True parameter theta for the uniform distribution
iterations = 1000  # Number of iterations to simulate
var_umvue <- c()  # Vector to store variance of UMVUE estimator
var_ube <- c()  # Vector to store variance of UBE estimator

# Loop over each sample size
for(n in range_of_n) {
  x1 <- c()  # Temporary vector to store UMVUE estimators
  x2 <- c()  # Temporary vector to store UBE estimators
  
  # Simulate data for the given number of iterations
  for(i in 1:iterations) {
    x <- runif(n, 0, theta)  # Generate 'n' random values from uniform(0, theta)
    xn <- max(x)  # Maximum of the sample
    xbar2 <- mean(x) * 2  # Twice the sample mean (used for UBE)
    
    # UMVUE estimator for exp(theta)
    func_of_xn <- exp(xn) + xn * exp(xn) / n
    
    # UBE estimator for exp(theta)
    func <- exp(xbar2)
    
    # Store the estimators in the respective vectors
    theta_umvue <- c(func_of_xn)
    theta_ube <- c(func)
    
    x1 <- c(x1, theta_umvue)  # Append UMVUE estimator to x1
    x2 <- c(x2, theta_ube)  # Append UBE estimator to x2
  }
  
  # Calculate variance for the UMVUE estimator
  v1 <- sum((x1 - exp(theta))^2) / iterations  # Variance formula for UMVUE
  
  # Calculate variance for the UBE estimator
  v2 <- sum((x2 - exp(theta))^2) / iterations  # Variance formula for UBE
  
  # Append variances to respective vectors
  var_umvue <- c(var_umvue, v1)
  var_ube <- c(var_ube, v2)
}

# Load ggplot2 library for plotting
library(ggplot2)

# Create a data frame to hold the results
data <- data.frame(
  n = rep(range_of_n, 2),  # Repeating sample sizes for both estimators
  variance = c(var_umvue, var_ube),  # Combine variances for both estimators
  estimator = rep(c("UMVUE", "UBE"), each = length(range_of_n))  # Label each estimator
)

# Plot the data using ggplot2
ggplot(data, aes(x = n, y = variance, color = estimator, group = estimator)) +
  geom_line(linewidth = 1) +  # Draw line for each estimator
  geom_point(size = 2) +  # Draw points for each sample size
  scale_y_continuous(trans = "log") +  # Apply natural log transformation on y-axis
  labs(
    title = "Variance of UMVUE vs UBE ",  # Title of the plot
    x = "Sample Size (n)",  # Label for x-axis
    y = "Variance (log scale)"  # Label for y-axis
  ) +
  theme_minimal() +  # Use minimal theme for better aesthetics
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    legend.title = element_blank()  # Remove legend title
  )
