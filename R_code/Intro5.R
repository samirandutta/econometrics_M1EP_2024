#Clear workspace 
rm(list=ls())

#===========================================================================================================================
# PBS6. Q1
#===========================================================================================================================

#Import data (use your own path!)
smoke_df <- read.csv("/Users/samirandutta/Library/CloudStorage/Dropbox/Teaching/IMetrics_E&P/IMetrics_2024/Datasets/smoke_data.csv")

# Regress educ on age and get residuals
m1 <- lm(educ ~ age, data = smoke_df)
smoke_df$res_m1 <- resid(m1)

# Regress cigs on age and get residuals
m2<- lm(cigs ~ age, data = smoke_df)
smoke_df$res_m2 <- resid(m2)

# FWL method
m3 <- lm(res_m2 ~ res_m1, data = smoke_df)
summary(m3)

# Direct regression of cigs on educ and age
m4 <- lm(cigs ~ educ + age, data = smoke_df)
summary(m4)

## Compare estimates

# Store
beta_direct <- summary(m4)$coefficients["educ", "Estimate"]
beta_fwl <- summary(m3)$coefficients["res_m1", "Estimate"]

# Print 
cat("beta_direct:", beta_direct, "\n")
cat("beta_fwl:", beta_fwl, "\n")


#===========================================================================================================================
# PBS6. Q2
#===========================================================================================================================

#Import data (use your own path!)
bw_df <- read.csv("/Users/samirandutta/Library/CloudStorage/Dropbox/Teaching/IMetrics_E&P/IMetrics_2024/Datasets/birthweight_data.csv")

#Check covariance 
cov <- cov(bw_df$smoker, bw_df$alcohol)
cov

# Model without interaction
r1 <- lm(birthweight ~ smoker + alcohol, data = bw_df)
summary(r1)

#Model with interaction 
r2 <- lm(birthweight ~ smoker + alcohol + smoker*alcohol, data = bw_df)
summary(r2)


#===========================================================================================================================
# PBS6. Q3
#===========================================================================================================================

#Import data (use your own path!)
wage_df <- read.csv("/Users/samirandutta/Library/CloudStorage/Dropbox/Teaching/IMetrics_E&P/IMetrics_2024/Datasets/wage1.csv")

#Scatter plot
scatter <- plot(wage_df$educ, wage_df$wage,
     main = "",
     xlab = "Years of Education",
     ylab = "Hourly Wage",
     pch = 19,         # Solid circles for points
     col = "blue")     # Set color for points
scatter

#Make new varb
wage_df$educsq <- wage_df$educ^2

#Reg
reg1 <- lm(wage ~ educ, data = wage_df)
summary(reg1)

reg2 <- lm(wage ~ educ + educsq, data = wage_df)
summary(reg2)

# Scatter plot with data points
plot(wage_df$educ, wage_df$wage,
     main = "",
     xlab = "Years of Education",
     ylab = "Hourly Wage",
     pch = 19,         # Solid circles for points
     col = "blue")     # Set color for points

# Add linear regression line (from reg1)
abline(reg1, col = "grey", lwd = 2, lty = 2)  # Linear regression line

# Add quadratic regression line (from reg2)
educ_range <- seq(min(wage_df$educ), max(wage_df$educ), length.out = 100)
educsq_range <- educ_range^2
predicted_wage <- predict(reg2, newdata = data.frame(educ = educ_range, educsq = educsq_range))

lines(educ_range, predicted_wage, col = "red", lwd = 2)  # Quadratic regression line

# Add legend
legend("topright", 
       legend = c("Linear Model", "Quadratic Model"), 
       col = c("grey", "red"), 
       lty = c(1, 1))



