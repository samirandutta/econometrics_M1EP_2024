# Clear workspace
rm(list=ls())

# Installing and loading packages 
install.packages("dplyr")
library(dplyr)

# Basic operations
x = 1
y = 2
a = x+y
m = x*y
s = x-y

# Vectors 
X <- c(0, 1, 2, 3)                       #RV outcomes in Q1
P_x <- c(1/8, 3/8, 3/8, 1/8)             #PDF from Q1

# Vector operations 
E_x <- sum(X * P_x)                      #E(X) = 0*1/8 + 1*3/8 + 2*3/8 + 3* 1/8
V_x <- sum(X^2 * P_x) - E_x^2            #V(X) = E(X^2) - [E(X)]^2

# Plot the PDF
plot1 <- plot(X, P_x, type = "h", lwd = 2, col = "red", 
     xlab = "x", ylab = "P(X=x)", 
     main = "Probability Distribution Function (PDF)")

# Calculate the CDF
cdf <- cumsum(P_x)

# Plot the CDF
plot2 <- plot(X, cdf, type = "s", lwd = 2, col = "blue", 
     xlab = "x", ylab = "CDF(x)", 
     main = "Cumulative Distribution Function (CDF)")

#Combine
plot_grid(plot1, plot2)


