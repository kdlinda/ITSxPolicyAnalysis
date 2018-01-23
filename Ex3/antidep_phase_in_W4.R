################################################
# Phase-in Periods
# Dora Linda Kocsis
# January 2017
################################################

# The code is based on: Lu C, et al. 
# Changes in antidepressant use by young people and suicidal behavior after 
# FDA warnings and media coverage: 
# quasi-experimental study BMJ 2014; 348: g3596
# http://www.bmj.com/content/348/bmj.g3596

# About: Time series of population rates are divided into three segments: 
# the pre-warning period (first quarter of 2000 to third quarter of 2003), 
# phase-in period (last quarter of 2003 to last quarter of 2004), 
# and post-warning period (first quarter of 2005 to last quarter of 2010). 

library(nlme)
library(car)

########################
# Read in the dataset
########################

df <- read.csv("*/antidep_phase_in.csv",header=T)
View(df)
summary(df)

########################
# Initial Plot
########################

# Plot outcome variable versus time
plot(df$time,df$ad_perc,
     ylab="Antidepressant Usage",
     ylim=c(0,2),
     xlab="Quarter",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis quarter labels
axis(1, at=1:44, labels=df$quarter)

# Add in the points for the figure
points(df$time,df$ad_perc,
       col="red",
       pch=20)

# Label the policy change
abline(v=15.5,lty=2)


#########################
# Create New Dataset
#########################

# Make a vector of the rows to be included
include <- c(1:15,21:44)

# Duplicate these rows into a new dataset
df_pi <- df[include,]

# Correct the trend variable in the new dataset
df_pi$trend[16:39] <- df_pi$trend[16:39] - 5

View(df_pi)

########################
# Modeling
########################

# A preliminary OLS regression
model_ols <- lm(ad_perc ~ time + level + trend, data=df_pi)
summary(model_ols)


################################
# Assessing Autocorrelation
################################

# Durbin-watson test, 12 time periods
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(df_pi$time,
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
# Note decay in ACF, significant spike at 4 in PACF, model p=4

# Reset the plot
par(mfrow=c(1,1))


########################
# Modeling - with phase-in period
########################

# Fit the GLS regression model
model_p5 <- gls(ad_perc ~ time + level + trend,
                data=df_pi,
                correlation=corARMA(p=5,form=~time),
                method="ML")
summary(model_p5)


########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(df$time,df$ad_perc,
     ylim=c(0,3),
     ylab="Antidepressant Usage",
     xlab="Quarter",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:44, labels=df$quarter)

# Add line indicating policy change
abline(v=15.5,lty=2)

# Plot the first line segment
lines(df$time[1:15], fitted(model_p5)[1:15], col="red",lwd=2)

# Plot the second line segment
lines(df$time[21:49], fitted(model_p5)[16:44], col="red",lwd=2)

# And the counterfactual
segments(16, model_p5$coef[1]+model_p5$coef[2]*16,
         35, model_p5$coef[1]+model_p5$coef[2]*44,
         lty=2, lwd=2, col='red')

# Add a box to show phase-in period
rect(15.5,-2,20.5,4, border = NA, col= '#00000011')

########################
# Modeling - without phase-in period
########################

# Fit the GLS regression model
model2_p5 <- gls(ad_perc ~ time + level + trend,
                data=df,
                correlation=corARMA(p=5,form=~time),
                method="ML")
summary(model2_p5)


########################
# Level Change Comparison
########################

lvl_comp <- model_p5$coef[3]-model2_p5$coef[3]
lvl_comp

########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(df$time,df$ad_perc,
     ylim=c(0,3),
     ylab="Antidepressant Usage",
     xlab="Quarter",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:44, labels=df$quarter)

# Add line indicating policy change
abline(v=15.5,lty=2)

# Plot the first line segment
lines(df$time[1:15], fitted(model2_p5)[1:15], col="red",lwd=2)

# Plot the second line segment
lines(df$time[16:44], fitted(model2_p5)[16:44], col="red",lwd=2)

# And the counterfactual
segments(16, model_p5$coef[1]+model_p5$coef[2]*16,
         35, model_p5$coef[1]+model_p5$coef[2]*44,
         lty=2, lwd=2, col='red')

# END
