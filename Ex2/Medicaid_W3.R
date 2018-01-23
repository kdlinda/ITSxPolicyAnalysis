################################################
# Interrupted Time Series Analysis with Control Group
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
df <- read.csv("*/antipsychotic_study_control.csv",header=T)

View(df)
summary(df)

# Setup variables
df$wv <- c(rep(1,19),rep(0,19))
df$time <- c(1:19,1:19)
df$level <- c(rep(0,8),rep(1,11),rep(0,8),rep(1,11))
df$trend <- c(rep(0,8),1:11,rep(0,8),1:11)
df$wvtime <- df$wv * df$time
df$wvlevel <- df$wv * df$level
df$wvtrend <- df$wv * df$trend


########################
# Initial Plot
########################

# Plot the time series for West Virginia
plot(df$time[1:19],df$market_share[1:19],
     ylab="Market share",
     ylim=c(0,0.54),
     xlab="Quarter",
     type="l",
     col="red",
     xaxt="n")

# Add in control group flow into Lake Huron
points(df$time[20:38],df$market_share[20:38],
       type='l',
       col="blue")

# Add x-axis year labels
axis(1, at=1:19, labels=df$yearqtr[1:19])

# Add in the points for the figure
points(df$time[1:19],df$market_share[1:19],
       col="red",
       pch=20)

points(df$time[20:38],df$market_share[20:38],
       col="blue",
       pch=20)

# Label the market share change
abline(v=8.5,lty=2)

# Add in a legend
legend(x=1, y=0.2, legend=c("West Virginia","Control States"),
       col=c("red","blue"),pch=20)

########################
# Modeling
########################

# A preliminary OLS regression
model_ols<-lm(market_share ~ time + wv + wvtime + level + trend + wvlevel + 
                wvtrend, data=df)
summary(model_ols)
confint(model_ols)


################################
# Assessing Autocorrelation
################################

# Durbin-watson test to 12 lags
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(df$time[1:19],
     residuals(model_ols)[1:19],
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce Plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')

########################
# Run the final model
########################

# Fit the GLS regression model
model_p1 <- gls(market_share ~ time + wv + wvtime + level + trend + wvlevel + 
                   wvtrend,
                 data=df,
                 correlation=corARMA(p=1,form=~time|wv),
                 method="ML")
summary(model_p1)
confint(model_p1)

########################
# Diagnostic tests
########################

# Likelihood-ratio tests to check whether the parameters of the AR process for the errors are necessary and sufficient
model_p1q1 <- update(model_p1,correlation=corARMA(q=1,p=1,form=~time|wv))
anova(model_p1,model_p1q1)

model_p2 <- update(model_p1,correlation=corARMA(p=2,form=~time|wv))
anova(model_p1,model_p2)

# Put plotting back to one chart
par(mfrow=c(1,1))

# Residual plot
qqPlot(residuals(model_p1))

########################
# Plot results
#########################

# First plot the raw data points for the Nile
plot(df$time[1:19],df$market_share[1:19],
     ylim=c(0,0.6),
     ylab="Market share",
     xlab="Quarter",
     pch=20,
     col="lightblue",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:19, labels=df$yearqrt[1:19])
# Label the policy change
abline(v=8.5,lty=2)

# Add in the points for the control
points(df$time[20:38],df$market_share[20:38],
       col="pink",
       pch=20)

# Plot the first line segment for the intervention group
lines(df$time[1:8], fitted(model_p1)[1:8], col="blue",lwd=2)

# Add the second line segment for the intervention group
lines(df$time[9:19], fitted(model_p1)[9:19], col="blue",lwd=2)

# Add the counterfactual for the intervention group
segments(9, model_p1$coef[1] + model_p1$coef[2]*9 + model_p1$coef[3]+model_p1$coef[4]*9 + 
           model_p1$coef[5] + model_p1$coef[6],
         19, model_p1$coef[1] + model_p1$coef[2]*19 + model_p1$coef[3]+model_p1$coef[4]*19 + 
           model_p1$coef[5] + model_p1$coef[6]*11,
         lty=2,col='blue',lwd=2)

# Plot the first line segment for the control group
lines(df$time[20:27], fitted(model_p1)[20:27], col="red",lwd=2)

# Add the second line segment for the control
lines(df$time[28:38], fitted(model_p1)[28:38], col="red",lwd=2)

# Add the counterfactual for the control group
segments(1, model_p1$coef[1]+model_p1$coef[2],
         19,model_p1$coef[1]+model_p1$coef[2]*60,
         lty=2,col='red',lwd=2)

# Add in a legend
legend(x=1, y=0.2, legend=c("West Virginia","Control States"), col=c("blue","red"),pch=20)


##############################################
# Predict absolute and relative changes
##############################################
# Case 1: 6 quarters after
# Predicted value
pred1 <- fitted(model_p1)[15]

# Counterfactual Estimate
cfac1 <- model_p1$coef[1] + model_p1$coef[2]*15 +
         model_p1$coef[3] + model_p1$coef[4]*15 +
         model_p1$coef[5] + model_p1$coef[6]*6

# Absolute change
pred1 - cfac1
# Relative change
(pred1 - cfac1) / cfac1


# Case 2: 2 years after
# Predicted value
pred2 <- fitted(model_p1)[16]

# Counterfactual Estimate
cfac2 <- model_p1$coef[1] + model_p1$coef[2]*16 +
         model_p1$coef[3] + model_p1$coef[4]*16 +
         model_p1$coef[5] + model_p1$coef[6]*8

# Absolute change
pred2 - cfac2
# Relative change
(pred2 - cfac2) / cfac2

