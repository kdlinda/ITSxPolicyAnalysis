################################################
# Wild Points
# Dora Linda Kocsis
# January 2017
################################################

# The code is based on:
# New Hampshire Medicaid Drug Cap Dataset

library(nlme)
library(car)

########################
# Read in the dataset
########################

data <- read.csv("C:/Users/dorak/Desktop/Policy Analysis Using ITS/Week 4/nh_wild_point.csv",header=T)

# Creating a new variable indicating the wild point
data$wp <- rep(0,31)
data$wp[20] <- 1
View(data)

########################
# Preliminary Analysis
########################

# Fit the OLS regression model including wild point
model_ols_wp <- lm(rxpp ~ time + level + trend + wp, data=data)
summary(model_ols_wp)

# Fit the OLS regression model without wild point
model_ols <- lm(rxpp ~ time + level + trend, data=data)
summary(model_ols)

diff <- model_ols_wp$coef[3]-model_ols$coef[3]
diff 
