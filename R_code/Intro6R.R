# Clear workspace
rm(list=ls())

# Read data
f_data <- read.csv("/Users/samirandutta/Library/CloudStorage/Dropbox/Teaching/IMetrics_E&P/IMetrics_2024/Datasets/fertility.csv")

# Q1. OLS Regression
#--------------------------------------------------------------------
ols <- lm(weeksm1 ~ morekids, data = f_data)
summary(ols)

# Q3. First Stage Regression
#--------------------------------------------------------------------
first_stage <- lm(morekids ~ samesex, data = f_data)
summary(first_stage)

# Get predicted values from the first stage
f_data$morekids_hat <- predict(first_stage)

# Second Stage Regression (Manual 2SLS)
#--------------------------------------------------------------------
second_stage <- lm(weeksm1 ~ morekids_hat, data = f_data)
summary(second_stage)

# IVREG for Verification
#--------------------------------------------------------------------

# Install and load packages
install.packages("AER")
library(AER)

iv_model <- ivreg(weeksm1 ~ morekids | samesex, data = f_data)
summary(iv_model)

# Compare Results
print("Comparison of Manual 2SLS and ivreg Results")
print(summary(second_stage)$coefficients)
print(summary(iv_model)$coefficients)
