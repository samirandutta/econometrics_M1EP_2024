#Clear workspace 
rm(list=ls())

# Set seed for reproducibility
set.seed(123)

# Define sample sizes and draws
sample_sizes <- c(2, 5, 10, 30, 50, 100)  # Different sample sizes
n_samples <- 1000                         # Number of samples to draw for each size

# Function to draw samples and calculate sample means 
draw_sample_means_unif <- function(sample_size, n_samples) {
  sample_means <- replicate(n_samples, mean(runif(sample_size, 0, 1)))
  return(sample_means)
}

# Initialize empty plot area
par(mfrow = c(2, 3))  # Arrange plots in a 2x3 grid

# Loop over each sample size, draw the samples, and plot the histogram
for (size in sample_sizes) {
  sample_means <- draw_sample_means_unif(size, n_samples) 
  hist(sample_means, breaks = 30, main = paste("n =", size),
       xlim = c(0, 1), col = "lightblue", xlab = "", 
       freq = FALSE, ylab = "Density")
  curve(dnorm(x, mean = 0.5, sd = sqrt(1/(12*size))), 
        col = "red", add = TRUE, lwd = 2)  # Add normal curve for comparison
}

# Function to draw samples and calculate sample means
draw_sample_means_exp <- function(sample_size, n_samples) {
  sample_means <- replicate(n_samples, mean(rexp(sample_size, rate = 1)))  # Exponential dist with rate=1
  return(sample_means)
}

# Loop over each sample size, draw the samples, and plot the histogram
for (size in sample_sizes) {
  sample_means <- draw_sample_means_exp(size, n_samples)
  hist(sample_means, breaks = 30, main = paste("n =", size),
       xlim = c(0, 3), col = "lightblue", xlab = "Sample Means", 
       freq = FALSE, ylab = "Density")
  curve(dnorm(x, mean = 1, sd = sqrt(1/size)),  # Theoretical normal distribution
        col = "red", add = TRUE, lwd = 2)  # Add normal curve for comparison
}



