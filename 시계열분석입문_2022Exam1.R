##################
# Basic Settings #
##################

setwd("/Users/hyungyeonghong/Desktop/TSA_exam1")
rm(list = ls(all = TRUE))
source("TS-library.R")

# Jan 2005 to Dec, 2018, hence it contains total 168 monthly data. 
data = read.csv("2022practice1.csv", header = F)
data = data$V2

data = ts(data, start = c(2005, 1), end = c(2018, 12), frequency = 12)
n = length(data)

##########################################################################
# (a) Time plot, correlograms (ACF) and discuss key features of the data #
##########################################################################

par(mfrow = c(1, 2))

plot.ts(data)
title("Time Plot of the Data")

acf2(data, lag = 100) 
title("SACF of the Data")

######################################
# (b) Removing trend and seasonality #
######################################

##############
# Regression #
##############

#########################################
# Removing Trend: Polynomial Regression #
#########################################

x = seq(from = 1, to = n, by = 1)
x2 = x^2
x3 = x^3

out.polynomial1 = lm(data ~ 1 + x)
out.polynomial2 = lm(data ~ 1 + x + x2)
out.polynomial3 = lm(data ~ 1 + x + x2 + x3)


AIC(out.polynomial1) # 1083.086
AIC(out.polynomial2) # 1081.771
AIC(out.polynomial3) # 1079.755


par(mfrow = c(2, 2))

plot.ts(data)
lines(x = as.vector(time(data)), y = out.polynomial3$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

plot(x = as.vector(time(data)), y = out.polynomial3$residuals, type = "l")
title("Residuals - Polynomial Regression")

acf2(out.polynomial3$residuals)
title("SACF of the Residuals - Polynomial Regression")

qqnorm(out.polynomial3$residuals)
qqline(out.polynomial3$residuals)

out.polynomial = lm(data ~ 1 + x + x2 + x3)

#############################################
# Removing Seasonality: Harmonic Regression #
#############################################

t = 1:n
d = 12
  
f1 = n/d
f2 = 2*f1
f3 = 3*f1
f4 = 4*f1
f5 = 5*f1
f6 = 6*f1

costerm1 = cos(f1*2*pi/n*t); sinterm1 = sin(f1*2*pi/n*t);
costerm2 = cos(f2*2*pi/n*t); sinterm2 = sin(f2*2*pi/n*t);
costerm3 = cos(f3*2*pi/n*t); sinterm3 = sin(f3*2*pi/n*t);
costerm4 = cos(f4*2*pi/n*t); sinterm4 = sin(f4*2*pi/n*t);
costerm5 = cos(f5*2*pi/n*t); sinterm5 = sin(f5*2*pi/n*t);
costerm6 = cos(f6*2*pi/n*t); sinterm6 = sin(f6*2*pi/n*t);


# Setting k = 1
out.harmonic1 = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1)
summary(out.harmonic1)

# Setting k = 2
out.harmonic2 = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2)
summary(out.harmonic2)

# Setting k = 3
out.harmonic3 = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
summary(out.harmonic3)

# Setting k = 4
out.harmonic4 = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3 + costerm4 + sinterm4)
summary(out.harmonic4)

AIC(out.harmonic1) # 815.9302
AIC(out.harmonic2) # 742.3954
AIC(out.harmonic3) # 728.71
AIC(out.harmonic4) # 714.2348

# Variable selection to choose k: stepwise selection
out.total = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3 +
                 costerm4 + sinterm4 + costerm5 + sinterm5 + costerm6 + sinterm6)

step(out.total, direction = "both")

out.stepwise = lm(out.polynomial$residuals ~ costerm1 + sinterm1 + 
                    costerm2 + sinterm2 + costerm3 + sinterm3 + costerm4 + sinterm4 + 
                    sinterm5 + costerm6)

AIC(out.stepwise) # 608.4239

par(mfrow = c(3, 2))

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
lines(x = as.vector(time(data)), y = out.harmonic1$fitted.values, col = "salmon")
title("Harmonic Regression with k = 1")

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
lines(x = as.vector(time(data)), y = out.harmonic2$fitted.values, col = "lightblue")
title("Harmonic Regression with k = 2")

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
lines(x = as.vector(time(data)), y = out.harmonic3$fitted.values, col = "lightgreen")
title("Harmonic Regression with k = 3")

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
lines(x = as.vector(time(data)), y = out.harmonic4$fitted.values, col = "orange")
title("Harmonic Regression with k = 4")

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
lines(x = as.vector(time(data)), y = out.stepwise$fitted.values, col = "pink")
title("Harmonic Regression with Stepwise Selection")

par(mfrow = c(2, 2))

plot(x = as.vector(time(data)), y = out.stepwise$residuals, type = 'l', xlab = "Time", ylab = "Residuals")
title("Residuals - Polynomial & Harmonic Regression")

acf2(out.stepwise$residuals, lag = 100)
title("SACF of Residuals - Polynomial & Harmonic Regression")

plot(out.stepwise$fitted.values, out.stepwise$residuals, xlab = "Fitted Values", ylab = "Residuals")
title("Residuals vs Fitted - Polynomial & Harmonic Regression")

qqnorm(out.stepwise$residuals)
qqline(out.stepwise$residuals)

#################################################################
# Smoothing : Smoothing-Based Classical Decomposition Algorithm #
#################################################################

#############################
# Removing Trend: MA filter #
#############################

h.ma = optimize(f = ma.cv, interval = c(5, n/2), Y = data, l = 1, tol = .Machine$double.eps^0.25)
h.optimal = round(h.ma$minimum)
out.ma = smooth.ma(data, h.optimal)

par(mfrow = c(2, 2))

plot.ts(data)
lines(x = as.vector(time(data)), out.ma, col = "red")
title("Estimated Trend - MA Filter")

plot.ts(data - out.ma)
title("Residuals - MA Filter")

acf2(data - out.ma)
title("SACF of the Residuals Obtained by MA Filter")

qqnorm(data - out.ma)
qqline(data - out.ma, col="red")

############################################
# Removing Seasonality: Seasonal Smoothing #
############################################

temp.detrend = data - out.ma

season.avg = season(temp.detrend, d = 12)

par(mfrow = c(2, 2))

plot.ts(temp.detrend)
lines(x = as.vector(time(temp.detrend)), y = season.avg + mean(temp.detrend), col = "red")
title("Seasonal Smoothing")

plot(x = as.vector(time(temp.detrend)), y = temp.detrend - season.avg - mean(temp.detrend), type = "l")
title("Residuals - MA filter & Seasonal Smoothing")

acf2(temp.detrend - season.avg - mean(temp.detrend), lag = 100)
title("SACF of the Residuals - MA filter & Seasonal Smoothing")

qqnorm(temp.detrend - season.avg - mean(temp.detrend))
qqline(temp.detrend - season.avg - mean(temp.detrend))

##############################################
# Re-estimating Trend: Polynomial Regression #
##############################################

data.deseasonalized = data - season.avg - mean(data)
plot.ts(data.deseasonalized)

x = seq(from = 1, to = n, by = 1)
x2 = x^2
x3 = x^3

out.smooth.reg = lm(data.deseasonalized ~ 1 + x + x2 + x3)
summary(out.smooth.reg)

par(mfrow = c(1, 1))

plot.ts(data.deseasonalized)
lines(x = as.vector(time(data.deseasonalized)), y = out.smooth.reg$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression with Deseasonalized Series")

par(mfrow = c(2, 2))

plot(x = as.vector(time(data.deseasonalized)), y = out.smooth.reg$residuals, type = "l",
     xlab = "Time", ylab = "Residuals")
title("Residuals - Polynomial Regression with Deseasonalized Series")

acf2(out.smooth.reg$residuals, lag = 100)
title("SACF of the Residuals - Polynomial Regression with Deseasonalized Series")

plot(x = out.smooth.reg$fitted.values, y = out.smooth.reg$residuals,
     xlab = "Fitted Values", ylab = "Residuals")
title("Residuals vs Fitted - Polynomial Regression with Deseasonalized Series")

qqnorm(out.smooth.reg$residuals)
qqline(out.smooth.reg$residuals)


################
# Differencing #
################

##############################################
# Removing seasonality by lag-d differencing #
##############################################

d = 12
season.diff = diff(data, lag = d)

par(mfrow = c(1, 3))

plot(season.diff, type = "l")
title("Residuals - Seasonal Differencing") # includes first order differencing

acf2(season.diff, lag = 100)
title("SACF of the Residuals - Seasonal Differencing")

qqnorm(season.diff)
qqline(season.diff)


##################################
# Removing trend by differencing #
##################################

data.diff = diff(diff(season.diff)) # apply diff() multiple times if needed

par(mfrow = c(1, 3))

plot.ts(data.diff)
title("Residuals - Differencing")

acf2(data.diff, lag = 100) # seasonality not fully removed
title("SACF of the Residuals - Differencing")

qqnorm(data.diff)
qqline(data.diff, col="red")

######################
# Test of Randomness #
######################

####################
# IID, WN Sequence #
####################

test(data.diff)

###############
# Gaussianity #
###############

shapiro.test(data.diff)

library(nortest)

ad.test(data.diff)
cvm.test(data.diff)
lillie.test(data.diff)

library(tseries)
jarque.bera.test(data.fiff)
jarque.bera.test(data.diff)
