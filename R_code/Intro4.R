# Clear workspace
rm(list=ls())

# Install stargazer if not already installed
# install.packages("stargazer")

# Load stargazer package
# library(stargazer)

# Import Data
data <- read.csv("E:/Dropbox/Teaching/IMetrics_E&P/IMetrics_2024/Datasets/wage1.csv")

#===========================================================================================================================
# Regress Wages on Female
#===========================================================================================================================

# Model 
reg1 <- lm(wage ~ female, data = data)

# Print results 
summary(reg1)

# Export as a LaTeX table to a file
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

#===========================================================================================================================
# Regress Wages on Education
#===========================================================================================================================

# Model
reg2 <- lm(wage ~ educ, data = data)

# Print results 
summary(reg2)

# Export as a LaTeX table to a file
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



