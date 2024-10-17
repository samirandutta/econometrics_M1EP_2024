# Clear workspace
rm(list=ls())

# Install stargazer package 
# install.packages("stargazer")

# Load stargazer package
# library(stargazer)

# Import Data
data <- read.csv("/Users/samirandutta/Library/CloudStorage/Dropbox/Teaching/IMetrics_E&P/IMetrics_2024/Datasets/wage1.csv")

#===========================================================================================================================
# PS #4
#===========================================================================================================================

# 1. Regress Wages on Female
#===========================================================================================================================

# Model 
reg1 <- lm(wage ~ female, data = data)

# Print results 
summary(reg1)

# Export as a LaTeX table 
# stargazer(reg1, type = "latex", title = "Regression Results",
          #dep.var.labels = "Wage", covariate.labels = "Female")

# Plot the wage data against femal
plot(data$female, data$wage,
     xlab = "Female Dummy (0 = Male, 1 = Female)",
     ylab = "Wage",
     pch = 19, col = "blue")

# Add the regression line to the plot
abline(reg1, col = "red", lwd = 2)

# Optional: Add legend
legend("topright", legend = c("Data points", "Regression line"), col = c("blue", "red"), pch = 19, lwd = 2)


# 2. Regress Wages on Education
#===========================================================================================================================

# Model
reg2 <- lm(wage ~ educ, data = data)

# Print results 
summary(reg2)

# Export as a LaTeX table 
#stargazer(reg2, type = "latex", title = "Regression Results",
          # dep.var.labels = "Wage", covariate.labels = "Education")

# Plot the wage data against education
plot(data$educ, data$wage,
     xlab = "Education",
     ylab = "Wage",
     pch = 19, col = "blue")

# Add the regression line to the plot
abline(reg2, col = "red", lwd = 2)

# Optional: Add legend
legend("topright", legend = c("Data points", "Regression line"), col = c("blue", "red"), pch = 19, lwd = 2)


#===========================================================================================================================
# PS #5
#===========================================================================================================================

# 2.Q3. Regress Wages on Education and Experience 
#===========================================================================================================================

# Model
reg3 <- lm(wage ~ educ + exper, data = data)

# Print results 
summary(reg3)


# 2.Q4 and Q5. Regress Wages on Education and Experience 
#===========================================================================================================================

# Calculate the correlation between 'educ' and 'exper'
covariance <- cov(data$educ, data$exper)

# Model
reg4 <- lm(wage ~ educ, data = data)

# Print results 
summary(reg4)


# 4.Q1  Regress Wages on tenure and female Dummy
#===========================================================================================================================

# Model
reg5 <- lm(wage ~ tenure + female, data = data)

# Print results 
summary(reg5)


# 4.Q3  Regress Wages on tenure and male dummy
#===========================================================================================================================

# Generate male variable
data$male <- 1 - data$female

# Model
reg5 <- lm(wage ~ tenure + male, data = data)

# Print results 
summary(reg5)


# 4.Q4  Regress Wages on tenure and both dummies (with intercept)
#===========================================================================================================================

# Model
reg6 <- lm(wage ~ tenure + male + female, data = data)

# Print results 
summary(reg6)

# 4.Q4  Regress Wages on tenure and both dummies (without intercept)
#===========================================================================================================================

# Model 
reg7 <- lm(wage ~ tenure + male + female - 1, data = data)

# Display the summary of the model
summary(reg7)



