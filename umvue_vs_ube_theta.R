# Generate uniform samples and calculate variances of UMVUE and UBE estimators

# Define different sample sizes for the simulation
range_of_n <- c(5,10,50,100,250,500,750,1000,2500,5000,10000)

# True parameter theta for the uniform distribution
theta = 4

# Number of iterations to simulate for each sample size
iterations = 1000

# Initialize vectors to store variance of UMVUE and UBE estimators
var_umvue <- c()
var_ube <- c()

# Loop over each sample size
for(n in range_of_n) {
  
  # Temporary vectors to store UMVUE and UBE estimators for current sample size
  x1 <- c()  # For UMVUE estimators
  x2 <- c()  # For UBE estimators
  
  # Simulate data for the given number of iterations
  for(i in 1:iterations) {
    
    # Generate 'n' random values from uniform(0, theta)
    x <- runif(n, 0, theta)
    
    # Compute the maximum value of the sample (for UMVUE calculation)
    xn <- max(x)
    
    # Compute twice the sample mean (used for UBE calculation)
    xbar2 <- mean(x) * 2
    
    # UMVUE estimator for theta
    func_of_xn <- xn * (n + 1) / n
    
    # UBE estimator for theta
    func <- xbar2
    
    # Store the estimators in the respective vectors
    theta_umvue <- c(func_of_xn)  # UMVUE estimator for the current iteration
    theta_ube <- c(func)  # UBE estimator for the current iteration
    
    # Append the estimators to the temporary vectors
    x1 <- c(x1, theta_umvue)  # Append UMVUE estimator to x1
    x2 <- c(x2, theta_ube)  # Append UBE estimator to x2
  }
  
  # Calculate variance for the UMVUE estimator
  # Variance formula: sum of squared differences from the true value, divided by the number of iterations
  v1 <- sum((x1 - theta)^2 / iterations)
  
  # Calculate variance for the UBE estimator
  # Variance formula: sum of squared differences from the true value, divided by the number of iterations
  v2 <- sum((x2 - theta)^2 / iterations)
  
  # Append calculated variances to the respective vectors
  var_umvue <- c(var_umvue, v1)
  var_ube <- c(var_ube, v2)
}

# Load ggplot2 library for plotting
library(ggplot2)

# Create a data frame to hold the results for plotting
data <- data.frame(
  n = rep(range_of_n, 2),  # Repeating sample sizes for both estimators
  variance = c(var_umvue, var_ube),  # Combine variances for both estimators
  estimator = rep(c("UMVUE", "UBE"), each = length(range_of_n))  # Label each estimator type
)

# Plot the data using ggplot2
ggplot(data, aes(x = n, y = variance, color = estimator, group = estimator)) +
  geom_line(linewidth = 1) +  # Draw line for each estimator to show variance trend
  geom_point(size = 2) +  # Draw points for each sample size to highlight data points
  scale_y_continuous(trans = "log") +  # Apply natural log transformation on y-axis for better visualization
  labs(
    title = "Variance of UMVUE vs UBE",  # Title of the plot
    x = "Sample Size (n)",  # Label for the x-axis
    y = "Variance (log scale)"  # Label for the y-axis
  ) +
  theme_minimal() +  # Use a minimal theme for clean aesthetics
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    legend.title = element_blank()  # Remove the legend title for a cleaner look
  )
