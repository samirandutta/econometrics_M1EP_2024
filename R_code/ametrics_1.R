# Clear workspace
rm(list=ls())

# Required packages 
# Install these packages before library
library(ggplot2)
library(cowplot)
library(dplyr)

# Random seed
set.seed(19)

# ------------------------------------------------------------------------------------------------------------
# Q1. Generating Data
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#1. Preliminaries 
# ------------------------------------------------------------------------------

# Sample size 
N <- 40

# Define x_i 
x <- c(rep(10, 20), rep(20, 20))

# Population parameters 
beta_0 <- 100
beta_1 <- 10

# Generate regression function/CEF i.e., y|x 
cef <- beta_0 + beta_1*x

# Random population error (i.i.d)
# They should have (i) mean 0, (ii) constant variance, 
# (iii) be uncorrelated with one another.
sigma2 <- 2500
e <- rnorm(40, mean = 0, sd = sqrt(sigma2))

#2. Theoretical distribution of y | x
# ------------------------------------------------------------------------------

# Range (for visual reasons)
range_y_give_x <- seq(30, 500, length.out = 300)

# Compute densities for each x value
pdf_x10 <- dnorm(range_y_give_x, mean = beta_0 + beta_1 * 10, sd = sqrt(sigma2))
pdf_x20 <- dnorm(range_y_give_x, mean = beta_0 + beta_1 * 20, sd = sqrt(sigma2))

# Create a data frame for ggplot
density_df <- data.frame(
  y = rep(range_y_give_x, 2),
  density = c(pdf_x10, pdf_x20),
  x_value = rep(c("10", "20"), each = length(range_y_give_x))
)

# Plot theoretical probability distributions
th_y <- ggplot(density_df, aes(x = y, y = density, color = x_value)) +
  geom_line(size = 1.2) +
  labs(
    title = "",
    x = expression(y[i]),
    y = "Density",
    color = expression(x[i])
  ) +
  scale_color_manual(values = c("10" = "red", "20" = "blue")) + 
  geom_vline(xintercept = 200) +
  geom_vline(xintercept = 300) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Display plot 
th_y

#3. Compute theoretical standard deviations
# ------------------------------------------------------------------------------

#3.1. For beta_1
# ------------------------------------------------------------------------------

# Variance
var_beta_1 <- sigma2/(sum(  (x-mean(x))^2 ) )

# Standard deviation
sd_beta_1 <- sqrt(var_beta_1)

# Plot theoretical distribution 
# ------------------------------

# Define range 
beta_1_vals <- seq(beta_1 - 4 * sd_beta_1, beta_1 + 4 * sd_beta_1, length.out = 300)

# Compute density 
density_vals <- dnorm(beta_1_vals, mean = beta_1, sd = sd_beta_1)

# Generate dataframe 
density_df <- data.frame(beta_1 = beta_1_vals, density = density_vals)

# Plot the theoretical sampling distribution of beta_1
beta_1_dens <- ggplot(density_df, aes(x = beta_1, y = density)) +
  geom_line(color = "blue", size = 1.2) +
  labs(
    title = "",
    x = expression(hat(beta)[1]),
    y = "Density"
  ) +
  geom_vline(xintercept = beta_1) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Display plot
beta_1_dens

#3.2. For beta_0
# ------------------------------------------------------------------------------

# Variance
var_beta_0 <- (sigma2*sum(x^2)) /( N*sum( (x-mean(x))^2 ) )

# Standard deviation
sd_beta_0 <- sqrt(var_beta_0)

# Plot theoretical distribution 
# ------------------------------

# Define range 
beta_0_vals <- seq(beta_0 - 4 * sd_beta_0, beta_0 + 4 * sd_beta_0, length.out = 300)

# Compute density
density_vals <- dnorm(beta_0_vals, mean = beta_0, sd = sd_beta_0)

# Generate dataframe 
density_df <- data.frame(beta_0 = beta_0_vals, density = density_vals)

# Plot the theoretical sampling distribution of beta_0
beta_0_dens <- ggplot(density_df, aes(x = beta_0, y = density)) +
  geom_line(color = "red", size = 1.2) +
  labs(
    title = "",
    x = expression(hat(beta)[0]),
    y = "Density"
  ) +
  geom_vline(xintercept = beta_0) + 
  theme_bw() +
  theme(panel.grid = element_blank())

# Display plot
beta_0_dens

# Visualise together
plot_grid(beta_1_dens, beta_0_dens)

#3.3 Covariance between beta_0 and beta_1 
# ------------------------------------------------------------------------------
cov_beta_0_beta_1 <- ( -sigma2*mean(x) )/ ( sum( (x-mean(x))^2 ) )


# ------------------------------------------------------------------------------------------------------------
# Q2. Generating a Sample and Running Regression
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#1. Generating a Sample
# -------------------------------------------------------------------------------------------------------------
set.seed(202)

# Add random noise to x values
x_sample <- x + rnorm(N, mean = 0, sd = 50)

# Generate y based on the true regression function + random error
y_sample <- beta_0 + beta_1 * x_sample + rnorm(N, mean = 0, sd = sqrt(sigma2))

# Create a data frame
data_sample <- data.frame(x = x_sample, y = y_sample)

# Run the linear regression
reg_model <- lm(y ~ x, data = data_sample)

# Results summary
summary(reg_model)

# Extract estimated beta_1
coef(reg_model)[2]

#2. Computing the estimated variances and covariances of estimators 
# -------------------------------------------------------------------------------------------------------------

# (i) Manually computations 
# ---------------------------

# Compute estimated residuals
e_hat <- reg_model$residuals

# Compute estimated variance of errors (sigma^2)
sigma2_hat <- sum(e_hat^2) / (N - 2)

# Compute variance of beta_1_hat
var_beta_1_hat <- sigma2_hat / sum((x_sample - mean(x_sample))^2)

# Compute variance of beta_0_hat
var_beta_0_hat <- sigma2_hat * (sum(x_sample^2) / (N * sum((x_sample - mean(x_sample))^2)))

# Compute covariance between beta_0_hat and beta_1_hat
cov_beta_0_beta_1_hat <- (-sigma2_hat * mean(x_sample)) / sum((x_sample - mean(x_sample))^2)

# Display results
cat("Variance of beta_1_hat (manual):", var_beta_1_hat, "\n")
cat("Variance of beta_0_hat (manual):", var_beta_0_hat, "\n")
cat("Covariance between beta_0_hat and beta_1_hat (manual):", cov_beta_0_beta_1_hat, "\n")

# (ii) Using R in-built command
# ------------------------------

# Extract estimated variance of errors 
sigma2_hat <- summary(reg_model)$sigma^2

# Extract the variance and covariances
vcov_matrix <- vcov(reg_model)  
vcov_matrix


# -------------------------------------------------------------------------------------------------------------------------
# Q3. Monte-Carlo Simulation
#  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#1. Fixed Sample Size, N = 50
#  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Preliminaries 
# ------------------------------------------------------------------------------

# Number of simulations and sample size 
num_sim <- 1000
sample_size <- 50

# Population parameters 
beta_0 <- 100
beta_1 <- 10

# Generate a fixed sample for x
# Note: We could also generate a new random sample for x within the Monte-Carlo
# simulation, as you might have done in the S1 take-home exam. But a fixed sample
# for x also works. 
x <- rnorm(sample_size, mean = 50, sd = 10)

# Initialize empty list 
beta_1_hat <- numeric(num_sim)

# Main Loop
# ------------------------------------------------------------------------------
for (i in 1:num_sim) {
  
  # Setting a different seed in each simulation
  set.seed(123 + 10*i) 
  
  # Generate y sample 
  # Note: all variability in y is due to variability in error
  y <- beta_0 + beta_1 * x + rnorm(sample_size, mean = 0, sd = 50)
  
  # Regression
  reg_model <- lm(y ~ x)
  
  # Store estimated beta_1 coefficient
  beta_1_hat[i] <- coef(reg_model)[2]
}

# Plot beta_1_hat density
# ------------------------------------------------------------------------------

# Convert values to dataframe
beta_1_df <- data.frame(beta_1_hat)

# Plot beta_1_hat density
ggplot(beta_1_df, aes(x = beta_1_hat)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = beta_1, color = "red") +
  labs(title = "",
       x = expression(hat(beta)[1]),
       y = "Density") +
  theme_bw() +
  theme(panel.grid = element_blank())


# Results
# ------------------------------------------------------------------------------

#1. Sample mean
mean_beta_1_hat <- sum(beta_1_hat)/num_sim                                      # Manual
mean_beta_1_hat <- mean(beta_1_hat)                                             # Direct R command 


#2. Sample variance
var_beta_1_hat <- sum((beta_1_hat - mean_beta_1_hat)^2) / (num_sim - 1)         # Manual
var_beta_1_hat <- var(beta_1_hat)                                               # Direct R command


#2. Varying Sample Size, N = 50, 100, 200, 500 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Preliminaries 
# ------------------------------------------------------------------------------
sample_sizes <- c(50, 100, 500, 1000)  
results <- data.frame()

# Loop over different sample sizes
# ------------------------------------------------------------------------------
for (n in sample_sizes) {
  
  # Generate fixed sample for x 
  x <- rnorm(n, mean = 50, sd = 10)
  
  # Initialize vector to store estimated beta_1 coefficients
  beta_1_hat <- numeric(num_sim)
  
  # Monte Carlo simulation
  for (i in 1:num_sim) {
    
    # Setting a different seed in each simulation
    set.seed(123 + 10*i) 
    
    # Generate y sample
    y <- beta_0 + beta_1 * x + rnorm(n, mean = 0, sd = 50)
    
    # Regression
    reg_model <- lm(y ~ x)
    
    # Store estimated beta_1 coefficient
    beta_1_hat[i] <- coef(reg_model)[2]
  }
  
  # Store results in dataframe
  results <- rbind(results, data.frame(
    sample_size = rep(n, num_sim),
    beta_1_hat = beta_1_hat
  ))
}

# Plot beta_1_hat density for different sample sizes in the same plot
ggplot(results, aes(x = beta_1_hat, color = factor(sample_size))) +
  geom_density(size = 0.7, show.legend = TRUE, key_glyph = "path") +  
  geom_vline(xintercept = beta_1, color = "grey", size = 1) +
  labs(title = "",
       x = expression(hat(beta)[1]),
       y = "Density",
       color = "Sample Size") +  # Rename legend title
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "maroon", "black")) +  # Custom colors
  theme_bw() +
  theme(panel.grid = element_blank())


# Compute summary statistics
diffsample_summary_stats <- results %>%
  group_by(sample_size) %>%
  summarise(
    mean_beta_1_hat = mean(beta_1_hat),
    var_beta_1_hat = var(beta_1_hat)
  )

# Display summary statistics
diffsample_summary_stats

#1. Varying sd of error
#  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Preliminaries 
# ------------------------------------------------------------------------------
sd_values <- c(50, 60, 70, 80)  
results <- data.frame()

# Loop over different values of sd for error
# ------------------------------------------------------------------------------
for (d in sd_values) {
  
  # Generate fixed sample for x 
  x <- rnorm(n, mean = 50, sd = 10)
  
  # Initialize vector to store estimated beta_1 coefficients
  beta_1_hat <- numeric(num_sim)
  
  # Monte Carlo simulation
  for (i in 1:num_sim) {
    
    # Setting a different seed in each simulation
    set.seed(123 + i) 
    
    # Generate y sample
    y <- beta_0 + beta_1 * x + rnorm(n, mean = 0, sd = d)
    
    # Regression
    reg_model <- lm(y ~ x)
    
    # Store estimated beta_1 coefficient
    beta_1_hat[i] <- coef(reg_model)[2]
  }
  
  # Store results in dataframe
  results <- rbind(results, data.frame(
    sd_values = rep(d, num_sim),
    beta_1_hat = beta_1_hat
  ))
}

# Plot beta_1_hat density for different sd
ggplot(results, aes(x = beta_1_hat, color = factor(sd_values))) +
  geom_density(size = 0.7, show.legend = TRUE, key_glyph = "path") +  
  geom_vline(xintercept = beta_1, color = "grey", size = 1) +
  labs(title = "",
       x = expression(hat(beta)[1]),
       y = "Density",
       color = "Standard Deviation of error") +  # Rename legend title
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "maroon", "black")) +  # Custom colors
  theme_bw() +
  theme(panel.grid = element_blank())


# Compute summary statistics
diffsd_summary_stats <- results %>%
  group_by(sd_values) %>%
  summarise(
    mean_beta_1_hat = mean(beta_1_hat),
    var_beta_1_hat = var(beta_1_hat)
  )

# Display summary statistics
diffsd_summary_stats

#3. Varying Number of Simulations, S = 10, 500, 1000, 5000
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Preliminaries 
# ------------------------------------------------------------------------------
num_sim_values <- c(10, 500, 1000, 5000)  
results <- data.frame()


# Loop over different values of num_sim
# ------------------------------------------------------------------------------
for (num_sim in num_sim_values) {
  
  # Generate fixed sample for x 
  x <- rnorm(n, mean = 50, sd = 10)
  
  # Initialize vector to store estimated beta_1 coefficients
  beta_1_hat <- numeric(num_sim)
  
  # Monte Carlo simulation
  for (i in 1:num_sim) {
    
    # Setting a different seed in each simulation
    set.seed(123 + 10*i) 
    
    # Generate y sample
    y <- beta_0 + beta_1 * x + rnorm(n, mean = 0, sd = 50)
    
    # Regression
    reg_model <- lm(y ~ x)
    
    # Store estimated beta_1 coefficient
    beta_1_hat[i] <- coef(reg_model)[2]
  }
  
  # Store results in dataframe
  results <- rbind(results, data.frame(
    num_sim = rep(num_sim, num_sim),
    beta_1_hat = beta_1_hat
  ))
}

# Plot beta_1_hat density for different num_sim values
ggplot(results, aes(x = beta_1_hat, color = factor(num_sim))) +
  geom_density(size = 0.7, show.legend = TRUE, key_glyph = "path") +  
  geom_vline(xintercept = beta_1, color = "grey", size = 1) +
  labs(title = "",
       x = expression(hat(beta)[1]),
       y = "Density",
       color = "Number of Simulations") +  # Rename legend title
  scale_color_manual(values = c("blue", "red", "maroon", "black")) +  # Custom colors
  theme_bw() +
  theme(panel.grid = element_blank())

# Compute summary statistics
numsim_summary_stats <- results %>%
  group_by(num_sim) %>%
  summarise(
    mean_beta_1_hat = mean(beta_1_hat),
    var_beta_1_hat = var(beta_1_hat)
  )

# Display summary statistics
numsim_summary_stats







