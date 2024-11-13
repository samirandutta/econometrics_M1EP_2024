#Clear workspace 
rm(list=ls())

#===========================================================================================================================
# PBS6. Q1
#===========================================================================================================================

#Import data
smoke_df <- read.csv("/Users/samirandutta/Library/CloudStorage/Dropbox/Teaching/IMetrics_E&P/IMetrics_2024/Datasets/smoke_data.csv")

# Regress educ on age and get residuals
m1 <- lm(educ ~ age, data = smoke_df)
data$res_m1 <- resid(m1)

# Regress cigs on age and get residuals
m2<- lm(cigs ~ age, data = smoke_df)
data$res_m2 <- resid(m2)

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









