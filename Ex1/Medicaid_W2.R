################################################
# ITSx: Week 2: Interrupted Time Series Analysis
# Dora Linda Kocsis
# January 2017
################################################

# The code is based on:
# West Virginia Antipsychotic Market Share Dataset

library(nlme)
library(car)

########################
# Read in the dataset
########################
data <- read.csv("C:/Users/dorak/Desktop/Policy Analysis Using ITS/Week 2/antipsychotic_study.csv",header=T)

View(data)
summary(data)

########################
# Adding Additional Variables
########################
#Note: time of interruption - April 2003 - datapoint: 9

data$time <- 1:nrow(data)

n1 <- 8
n2 <- 11
l <- c(rep(0, n1), rep(1, n2))
data$level <- l 

t <- c(rep(0, n1), 1:11)
data$trend <- t

########################
# Initial Plot
########################

# Plot outcome variable versus time
plot(data$yearqtr,data$marketshare,
     ylab="Market Share in West Virginia",
     ylim=c(0,0.54),
     xlab="Quarter",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:19, labels=data$year)

# Add in the points for the figure
points(data$yearqtr,data$marketshare,
       col="red",
       pch=20)

# Label the market share change
abline(v=8.5,lty=2)

########################
# Modeling
########################

# A preliminary OLS regression
model_ols <- lm(marketshare ~ time + level + trend, data=data)
summary(model_ols)

# Durbin-watson test, 4 time periods
dwt(model_ols,max.lag=8,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(data$yearqtr,
     residuals(model_ols),
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

########################
# Run the final model
########################

# Fit the GLS regression model
model_p0 <- gls(marketshare ~ time + level + trend,
                 data=data,
                 correlation= NULL,
                 method="ML")
summary(model_p0)

# Likelihood-ratio tests to check AR process
model_p1 <- update(model_p0,correlation=corARMA(p=1,form=~time))
anova(model_p0,model_p1)

model_q1 <- update(model_p0,correlation=corARMA(q=1,form=~time))
anova(model_p0,model_q1)

# Residual plot
# Null Hypo: the residuals of a correctly specified model are independently distributed--the residuals are white noise
par(mfrow=c(1,1))
qqPlot(residuals(model_p0))

########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(data$time,data$marketshare,
     ylim=c(0,0.54),
     ylab="Market Share",
     xlab="Quarter",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:19, labels=data$yearqtr)

# Add line indicating market share pattern change
abline(v=8.5,lty="dotted")

# Plot the first line segment
lines(data$time[1:8], fitted(model_p0)[1:8], col="red",lwd=2)
# Plot the second line segment
lines(data$time[9:19], fitted(model_p0)[9:19], col="red",lwd=2)

# And the counterfactual
segments(1,
         model_p0$coef[1]+model_p0$coef[2],
         19,
         model_p0$coef[1]+model_p0$coef[2]*19,
         lty=2,
         lwd=2,
         col='red')


##############################################
# Predict absolute and relative changes
##############################################
# Case 1: 6 quarters after
# Predicted value
pred1 <- fitted(model_p0)[15]

# Counterfactual Estimate
cfac1 <- model_p0$coef[1] + model_p0$coef[2]*15

# Absolute change
pred1 - cfac1
# Relative change
(pred1 - cfac1) / cfac1


# Case 2: 2 years after
# Predicted value
pred2 <- fitted(model_p0)[16]

# Counterfactual Estimate
cfac2 <- model_p0$coef[1] + model_p0$coef[2]*16

# Absolute change
pred2 - cfac2
# Relative change
(pred2 - cfac2) / cfac2
