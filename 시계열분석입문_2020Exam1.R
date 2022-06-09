setwd("/Users/hyungyeonghong/Desktop/2022-1학기/TSA2022sp/exam1_practice")
rm(list = ls(all = TRUE))
source("TS-library.R")

# monthly time series observed from March, 1932 to Feb, 2010, hence it contains total 936 monthly data.
data = scan("2020exam1.csv")
data = ts(data, start = c(1932, 3), end = c(2010, 2), frequency = 12)
n = length(data)

# (a) Time plot, correlograms (ACF) and discuss key features of the data.
par(mfrow = c(1, 2))

plot.ts(data) # quadratic trend, frequent increasing-decreasing trend
title("Time Plot of the Data")
acf2(data) # significant correlations over all the lags, d = 12 seasonality
title("SACF of the Data")

acf(data, lag.max = 100) # linear decay, seasonality


# (b) Removing trend and seasonality

# (b)-1. Regression

# Removing trend by polynomial regression
x = seq(from = 1, to = n, by = 1)
x2 = x^2

out.polynomial = lm(data ~ 1 + x + x2)
summary(out.polynomial)

par(mfrow = c(1, 3))

plot.ts(data)
lines(x = as.vector(time(data)), y = out.polynomial$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
title("Residuals - Polynomial Regression")

acf2(out.polynomial$residuals)
title("SACF of the Residuals - Polynomial Regression")

# Still has quarternary trend in the residuals: Re-fit the model

x3 = x^3
x4 = x^4

out.polynomial = lm(data ~ 1 + x + x2 + x3 + x4)
summary(out.polynomial)

par(mfrow = c(1, 3))

plot.ts(data)
lines(x = as.vector(time(data)), y = out.polynomial$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
title("Residuals - Polynomial Regression")

acf2(out.polynomial$residuals)
title("SACF of the Residuals - Polynomial Regression")

# Removing seasnoality by harmonic regression
t = 1:n

f1 = n/12
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

AIC(out.harmonic1) # 15949.32
AIC(out.harmonic2) # 15277.87
AIC(out.harmonic3) # 15034.39
AIC(out.harmonic4) # 15009.66

# Variable selection to choose k: stepwise selection
out.total = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3 +
                 costerm4 + sinterm4 + costerm5 + sinterm5 + costerm6 + sinterm6)

step(out.total, direction = "both")

out.stepwise = lm(out.polynomial$residuals ~ costerm1 + sinterm1 + 
                    costerm2 + sinterm2 + costerm3 + sinterm4 + costerm5 + sinterm6)

AIC(out.stepwise) # 15004.38

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

plot(x = as.vector(time(data)), y = out.stepwise$residuals, type = 'l')
title("Residuals - Polynomial & Harmonic Regression")

acf2(out.stepwise$residuals)
title("SACF of Residuals - Polynomial & Harmonic Regression")

plot(out.stepwise$fitted.values, out.stepwise$residuals)
title("Residuals vs Fitted - Polynomial & Harmonic Regression")

qqnorm(out.stepwise$residuals)
qqline(out.stepwise$residuals)


# (b) - 2. Smoothing

# Removing trend by MA filter

# MA Filter
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

# Removing seasonality by seasonal smoothing

temp.detrend = data - out.ma

season.avg = season(temp.detrend, d = 12)

par(mfrow = c(2, 2))

plot.ts(temp.detrend)
lines(x = as.vector(time(temp.detrend)), y = season.avg + mean(temp.detrend), col = "red")
title("Seasonal Smoothing")

plot(x = as.vector(time(temp.detrend)), y = temp.detrend - season.avg - mean(temp.detrend), type = "l")
title("Residuals - MA filter & Seasonal Smoothing")

acf2(temp.detrend - season.avg - mean(temp.detrend))
title("SACF of the Residuals - MA filter & Seasonal Smoothing")

qqnorm(temp.detrend - season.avg - mean(temp.detrend))
qqline(temp.detrend - season.avg - mean(temp.detrend))

# Re-estimating and removing trend by OLS

data.deseasonalized = data - season.avg - mean(data)
plot.ts(data.deseasonalized)

out.smooth.reg = lm(data.deseasonalized ~ 1 + x + x2 + x3 + x4)
summary(out.smooth.reg)

par(mfrow = c(1, 1))

plot.ts(data.deseasonalized)
lines(x = as.vector(time(data.deseasonalized)), y = out.smooth.reg$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression with Deseasonalized Series")

par(mfrow = c(2, 2))

plot(x = as.vector(time(data.deseasonalized)), y = out.smooth.reg$residuals, type = "l")
title("Residuals - Polynomial Regression with Deseasonalized Series")

acf2(out.smooth.reg$residuals)
title("SACF of the Residuals - Polynomial Regression with Deseasonalized Series")

plot(x = out.smooth.reg$fitted.values, y = out.smooth.reg$residuals)
title("Residuals vs Fitted - Polynomial Regression with Deseasonalized Series")

qqnorm(out.smooth.reg$residuals)
qqline(out.smooth.reg$residuals)

# does not have heteroscedascity but the normality assumption seems to be violated
# still better than removing both trend and seasonality by regression

# (b) - 3. Differencing

# Removing seasonality by lag-12 differencing

season.diff12 = diff(data, lag = 12)

par(mfrow = c(1, 3))

plot(x = time(data)[13:n], season.diff12, type = "l")
title("Residuals - Seasonal Differencing") # includes first order differencing

acf2(season.diff12) # seasonality not fully removed
title("SACF of the Residuals - Seasonal Differencing")

qqnorm(season.diff12)
qqline(season.diff12)

# Removing trend by differencing

data.diff = diff(diff(diff(season.diff12))) # 과대차분 위험

par(mfrow = c(1, 3))

plot.ts(data.diff)
title("Residuals - Differencing")

acf2(data.diff) # seasonality not fully removed
title("SACF of the Residuals - Differencing")

qqnorm(data.diff)
qqline(data.diff, col="red")


# (b) - 4. Classical Decomposition Algorithm

out.decompose = classical(data, d = 12, order = 4)

par(mfrow = c(2, 2))

plot.ts(data)
lines(as.vector(time(data)), out.decompose$m1, col = "red")
title("Classical Decomposition Algorithm: Step 1")

plot.ts(data - out.decompose$m1)
lines(as.vector(time(data)), out.decompose$st, col = "red")
title("Classical Decomposition Algorithm: Step 2")

plot.ts(data - out.decompose$st)
lines(as.vector(time(data)), out.decompose$m, col = "red")
title("Classical Decomposition Algorithm: Step 3")

plot.ts(data)
lines(as.vector(time(data)), out.decompose$fit, col = "red")
title("Classical Decomposition Algorithm: Final Fit")

par(mfrow = c(1, 3))

plot(x = as.vector(time(data)), y = data - out.decompose$fit, type = "l")
title("Residuals - Classical Decmoposition Algorithm")

acf2(data - out.decompose$fit)
title("SACF of the Residuals - Classical Decmoposition Algorithm")

qqnorm(data - out.decompose$fit)
qqline(data - out.decompose$fit)

# Final Model: Smoothing
# Test of Randomness

# IID, WN Sequence
test(out.smooth.reg$residuals) # not iid

# Gaussianity
par(mfrow = c(1, 1))
qqnorm(out.smooth.reg$residuals)
qqline(out.smooth.reg$residuals)

shapiro.test(out.smooth.reg$residuals) # H0 Rejected: Does not follow normal distribution

library(nortest)

ad.test(out.smooth.reg$residuals) # H0 Rejected: Does not follow normal distribution
cvm.test(out.smooth.reg$residuals) # H0 Rejected: Does not follow normal distribution
lillie.test(out.smooth.reg$residuals) # H0 Rejected: Does not follow normal distribution

library(tseries)
jarque.bera.test(out.smooth.reg$residuals) # H0 Rejected: Does not follow normal distribution

# ADF test - reject H0, therefore stationary
adf.test(out.smooth.reg$residuals)



