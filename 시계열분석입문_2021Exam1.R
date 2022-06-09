setwd("/Users/hyungyeonghong/Desktop/2022-1학기/TSA2022sp/exam1_practice")
rm(list = ls(all = TRUE))
source("TS-library.R")

# weekly time series observed from Jan 10, 2010 to Aug 11, 2019, hence it contains total 500 weekly data
# yearly : 1, quarterly : 4, monthly : 12, weekly : 52

data = scan("2021practice1.csv")
data = ts(data, start = c(2010, 2), end = c(2019, 33), frequency = 52)
n = length(data)

# (a) Time plot, correlograms (ACF) and discuss key features of the data.
par(mfrow = c(1, 2))

plot.ts(data) # linear, quadratic or exponential trend, but not sure / has yearly seasonality
title("Time Plot of the Data")
acf2(data, lag = 100) # significant correlations over all the lags, d = 12 seasonality
title("SACF of the Data")

plot(decompose(data))

# (b) Removing trend and seasonality

# (b)-1. Regression

# Removing trend by polynomial regression
x = seq(from = 1, to = n, by = 1)

out.polynomial = lm(data ~ 1 + x)
summary(out.polynomial)

par(mfrow = c(1, 3))

plot.ts(data)
lines(x = as.vector(time(data)), y = out.polynomial$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
title("Residuals - Polynomial Regression")

acf2(out.polynomial$residuals)
title("SACF of the Residuals - Polynomial Regression")

# Still has quadratic trend in the residuals: Re-fit the model

x2 = x^2

out.polynomial = lm(data ~ 1 + x + x2)
summary(out.polynomial)

par(mfrow = c(1, 3))

plot.ts(data)
lines(x = as.vector(time(data)), y = out.polynomial$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

plot(x = as.vector(time(data)), y = out.polynomial$residuals, type = "l")
title("Residuals - Polynomial Regression")

acf2(out.polynomial$residuals, lag = 100) # yearly trend(1 year = 52 weeks)
title("SACF of the Residuals - Polynomial Regression")

# Removing seasnoality by harmonic regression
t = 1:n

f1 = n/52
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

AIC(out.harmonic1) # 8389.629
AIC(out.harmonic2) # 8383.824
AIC(out.harmonic3) # 8375.763
AIC(out.harmonic4) # 8379.59

# Variable selection to choose k: stepwise selection
out.total = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3 +
                 costerm4 + sinterm4 + costerm5 + sinterm5 + costerm6 + sinterm6)

step(out.total, direction = "both")

out.stepwise = lm(out.polynomial$residuals ~ costerm1 + sinterm1 + 
                    sinterm2 + costerm3 + sinterm3)

AIC(out.stepwise) # 8374.192

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


# b - (2). Smoothing

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

acf2(data - out.ma, lag = 100)
title("SACF of the Residuals - MA Filter")

qqnorm(data - out.ma)
qqline(data - out.ma, col="red")


# Removing seasonality by seasonal smoothing

temp.detrend = data - out.ma

season.avg = season(temp.detrend, d = 52)

par(mfrow = c(2, 2))

plot(x = as.vector(time(temp.detrend)), y = temp.detrend, type = "l",
     xlab = "Time", ylab = "Detrended Data")
lines(x = as.vector(time(temp.detrend)), y = season.avg + mean(temp.detrend), col = "red")
title("Seasonal Smoothing")

plot(x = as.vector(time(temp.detrend)), y = temp.detrend - season.avg - mean(temp.detrend), type = "l",
     xlab = "Time", ylab = " Detrended Data")
title("Residuals - MA filter & Seasonal Smoothing")

acf2(temp.detrend - season.avg - mean(temp.detrend), lag = 100)
title("SACF of the Residuals - MA filter & Seasonal Smoothing")

qqnorm(temp.detrend - season.avg - mean(temp.detrend))
qqline(temp.detrend - season.avg - mean(temp.detrend))

plot(decompose(temp.detrend))

# Re-estimating and removing trend by OLS

data.deseasonalized = data - season.avg - mean(data)

par(mfrow = c(1, 1))
plot.ts(data.deseasonalized)

out.smooth.reg = lm(data.deseasonalized ~ 1 + x + x2)
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


# (b) - 3. Classical Decomposition Algorithm

out.decompose = classical(data, d = 52, order = 2)

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

acf2(data - out.decompose$fit, lag = 100)
title("SACF of the Residuals - Classical Decmoposition Algorithm")

qqnorm(data - out.decompose$fit)
qqline(data - out.decompose$fit)


# (b) - 3. Differencing

# Removing seasonality by lag-12 differencing

season.diff52 = diff(data, lag = 52)

par(mfrow = c(1, 3))

plot(x = time(data)[53:n], season.diff52, type = "l")
title("Residuals - Seasonal Differencing") # includes first order differencing

acf2(season.diff52) # seasonality not fully removed
title("SACF of the Residuals - Seasonal Differencing")

qqnorm(season.diff52)
qqline(season.diff52)

# Removing trend by differencing

data.diff = diff(season.diff52) # 과대차분 위험

par(mfrow = c(1, 3))

plot.ts(data.diff)
title("Residuals - Differencing")

acf2(data.diff, lag = 446) # seasonality not fully removed
title("SACF of the Residuals - Differencing")

qqnorm(data.diff)
qqline(data.diff, col="red")


tseries::adf.test(data.diff)

length(data.diff)




