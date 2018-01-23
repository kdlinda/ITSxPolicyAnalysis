################################################
# Multiple Interventions
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

df <- read.csv("C:/Users/dorak/Desktop/Policy Analysis Using ITS/Week 4/nh_multiple_int.csv",header=T)
View(df)
summary(df)

########################
# Adding Additional Variables
########################
#Note: time of interruption 1 - Sep-81 - datapoint: 21 - 3-drug-cap
#      time of interruption 2 - Aug-82 - datapoint: 32 - $1 copayment

df$time <- 1:nrow(df)

df$dc_level <- c(rep(0,20), rep(1,28)) 
df$dc_trend <- c(rep(0,20), 1:28)

df$cp_level <- c(rep(0,31), rep(1,17))
df$cp_trend <- c(rep(0,31), 1:17)

# Creating a new variable indicating the wild point
df$wp <- c(rep(0,48))
df$wp[20] <- 1

########################
# Initial Plot
########################

# Plot outcome variable versus time
plot(df$time,df$rxpp,
     ylab="New Hampshire Drup Cap",
     ylim=c(0,6.5),
     xlab="Month",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:48, labels=df$month)

# Add in the points for the figure
points(df$time,df$rxpp,
       col="red",
       pch=20)

# Label the policy changes
abline(v=20.5,lty=2)
abline(v=31.5,lty=2)


########################
# Modeling
########################

# A preliminary OLS regression
model_ols <- lm(rxpp ~ time + dc_level + dc_trend + cp_level + cp_trend + wp, data=df)
summary(model_ols)


################################
# Assessing Autocorrelation
################################

# Durbin-watson test, 12 time periods
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(residuals(model_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')
# Model with p=0

# Reset plot window
par(mfrow=c(1,1))

########################
# Run the final model
########################

# Fit the GLS regression model
model_p0 <- gls(rxpp ~ time + dc_level + dc_trend + cp_level + cp_trend + wp,
                data=df,
                correlation=NULL,
                method="ML")
summary(model_p0)

########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(df$time,df$rxpp,
     ylab="New Hampshire Drug Cap",
     ylim=c(0,6.5),
     xlab="Month",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:48, labels=df$month)

# Add line indicating the policy changes
abline(v=20.5,lty=2)
abline(v=31.5,lty=2)

# Plot the first line segment
lines(df$time[1:20], fitted(model_p0)[1:20], col="red",lwd=2)

# Plot the second line segment
lines(df$time[21:31], fitted(model_p0)[21:31], col="red",lwd=2)

# Plot the third line segment
lines(df$time[32:48], fitted(model_p0)[32:48], col="red",lwd=2)

# And the first counterfactual
segments(21, model_p0$coef[1]+model_p0$coef[2]*21,
         31, model_p0$coef[1]+model_p0$coef[2]*31,
         lty=2, lwd=2, col='red')

# And the second counterfactual
segments(32, model_p0$coef[1] + model_p0$coef[2]*32 +
           model_p0$coef[3] + model_p0$coef[4]*12,
         48, model_p0$coef[1] + model_p0$coef[2]*48 +
           model_p0$coef[3] + model_p0$coef[4]*28,
         lty=2, lwd=2, col='red')


##############################################
# Predict absolute and relative changes
##############################################
# 1 year after
# Predicted value
pred <- fitted(model_p0)[43]

# Counterfactual Estimate
cfac <- model_p0$coef[1] + model_p0$coef[2]*43 +
        model_p0$coef[3] + model_p0$coef[4]*23

# Absolute change
pred - cfac
# Relative change
(pred - cfac) / cfac

# END
