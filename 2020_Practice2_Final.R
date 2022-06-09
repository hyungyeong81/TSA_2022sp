# 2020-Spring Practice Exam 2

######################
### Basic settings ###
######################

rm(list=ls(all=TRUE))

setwd("/Users/hyungyeonghong/Desktop/2022_Spring/TSA2022sp/exam2_practice")

data.original = read.csv("2020exam2.csv", head = FALSE) # change file name
head(data.original)

data.original = data.original$V1 # select a column
head(data.original)
length(data.original)

###############################
### Time Plot, Correlograms ###
###############################

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(data.original, type = "l"); title("Time Plot")
acf.plot = acf(data.original, lag = 50, plot = FALSE); plot(acf.plot, main = "SACF")
pacf.plot = pacf(data.original, lag = 50, plot = FALSE); plot(pacf.plot, main = "SPACF")

data = data.original # if you are going to use the original data


#############################
### Polynomial Regression ###
#############################

n = length(data)

x = seq(from = 1, to = n, by = 1) 
x2 = x^2
x3 = x^3
x4 = x^4

out.polynomial1 = lm(data ~ 1 + x)
out.polynomial2 = lm(data ~ 1 + x + x2)
out.polynomial3 = lm(data ~ 1 + x + x2 + x3)
out.polynomial4 = lm(data ~ 1 + x + x2 + x3 + x4)

AIC(out.polynomial1)
AIC(out.polynomial2)
AIC(out.polynomial3)
AIC(out.polynomial4)

BIC(out.polynomial1)
BIC(out.polynomial2)
BIC(out.polynomial3)
BIC(out.polynomial4)

MuMIn::AICc(out.polynomial1)
MuMIn::AICc(out.polynomial2)
MuMIn::AICc(out.polynomial3)
MuMIn::AICc(out.polynomial4)


# 1st Order Polynomial Regression
par(mfrow = c(1, 1))
plot.ts(data)
lines(x = 1:n, y = out.polynomial1$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

par(mfrow = c(2, 2))
plot(x = 1:n, y = out.polynomial1$residuals, type = "l")
title("Residuals - Polynomial Regression")
qqnorm(out.polynomial1$residuals)
qqline(out.polynomial1$residuals)
acf.plot = acf(out.polynomial1$residuals, lag = 50, plot = FALSE)
plot(acf.plot, main = "SACF of the Residuals - Polynomial Regression")
pacf.plot = pacf(out.polynomial1$residuals, lag = 50, plot = FALSE)
plot(pacf.plot, main = "SPACF of the Residuals - Polynomial Regression")


# 2nd Order Polynomial Regression
par(mfrow = c(1, 1))
plot.ts(data)
lines(x = 1:n, y = out.polynomial2$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

par(mfrow = c(2, 2))
plot(x = 1:n, y = out.polynomial2$residuals, type = "l")
title("Residuals - Polynomial Regression")
qqnorm(out.polynomial2$residuals)
qqline(out.polynomial2$residuals)
acf.plot = acf(out.polynomial2$residuals, lag = 50, plot = FALSE)
plot(acf.plot, main = "SACF of the Residuals - Polynomial Regression")
pacf.plot = pacf(out.polynomial2$residuals, lag = 50, plot = FALSE)
plot(pacf.plot, main = "SPACF of the Residuals - Polynomial Regression")


# 3rd Order Polynomial Regression
par(mfrow = c(1, 1))
plot.ts(data)
lines(x = 1:n, y = out.polynomial3$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

par(mfrow = c(2, 2))
plot(x = 1:n, y = out.polynomial3$residuals, type = "l")
title("Residuals - Polynomial Regression")
qqnorm(out.polynomial3$residuals)
qqline(out.polynomial3$residuals)
acf.plot = acf(out.polynomial3$residuals, lag = 50, plot = FALSE)
plot(acf.plot, main = "SACF of the Residuals - Polynomial Regression")
pacf.plot = pacf(out.polynomial3$residuals, lag = 50, plot = FALSE)
plot(pacf.plot, main = "SPACF of the Residuals - Polynomial Regression")


# 4th Order Polynomial Regression
par(mfrow = c(1, 1))
plot.ts(data)
lines(x = 1:n, y = out.polynomial4$fitted.values, col = "red")
title("Estimated Trend - Polynomial Regression")

par(mfrow = c(2, 2))
plot(x = 1:n, y = out.polynomial4$residuals, type = "l")
title("Residuals - Polynomial Regression")
qqnorm(out.polynomial4$residuals)
qqline(out.polynomial4$residuals)
acf.plot = acf(out.polynomial4$residuals, lag = 50, plot = FALSE)
plot(acf.plot, main = "SACF of the Residuals - Polynomial Regression")
pacf.plot = pacf(out.polynomial4$residuals, lag = 50, plot = FALSE)
plot(pacf.plot, main = "SPACF of the Residuals - Polynomial Regression")


# Final polynomial regression model
out.polynomial = lm(data ~ 1 + x + x2) # choose the final polynomial regression model



###########################
### Harmonic Regression ###
###########################

t = 1:n
d = 4 # change the seasonal period

f1 = n/d
f2 = 2*f1
f3 = 3*f1
f4 = 4*f1

costerm1 = cos(f1*2*pi/n*t); sinterm1 = sin(f1*2*pi/n*t)
costerm2 = cos(f2*2*pi/n*t); sinterm2 = sin(f2*2*pi/n*t)
costerm3 = cos(f3*2*pi/n*t); sinterm3 = sin(f3*2*pi/n*t)
costerm4 = cos(f4*2*pi/n*t); sinterm4 = sin(f4*2*pi/n*t)


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


AIC(out.harmonic1)
AIC(out.harmonic2)
AIC(out.harmonic3)
AIC(out.harmonic4)


# Stepwise Selection
out.total = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3 + costerm4 + sinterm4)
step(out.total, direction = "both")


# Final harmonic regression model
out.harmonic = lm(out.polynomial$residuals ~ 1 + costerm1 + sinterm1) # choose the final polynomial regression model

par(mfrow = c(2, 2))
plot(x = 1:n, y = out.harmonic$residuals, type = "l")
title("Residuals - Polynomial & Harmonic Regression")
qqnorm(out.harmonic$residuals)
qqline(out.harmonic$residuals)
acf.plot = acf(out.harmonic$residuals, lag = 50, plot = FALSE)
plot(acf.plot, main = "SACF of the Residuals - Polynomial & Harmonic Regression")
pacf.plot = pacf(out.harmonic$residuals, lag = 50, plot = FALSE)
plot(pacf.plot, main = "SPACF of the Residuals - Polynomial & Harmonic Regression")



##########################################################
### Final Residual Plot(Trend and Seasonality Removed) ###
##########################################################

par(mfrow = c(2, 2))
plot(x = 1:n, y = out.polynomial$residuals, type = "l") # change the fitted model
title("Residuals - Regression")
qqnorm(out.polynomial$residuals) # change the fitted model
qqline(out.polynomial$residuals) # change the fitted model
acf.plot = acf(out.polynomial$residuals, lag = 50, plot = FALSE) # change the fitted model
plot(acf.plot, main = "SACF of the Residuals - Regression")
pacf.plot = pacf(out.polynomial$residuals, lag = 50, plot = FALSE) # change the fitted model
plot(pacf.plot, main = "SPACF of the Residuals - Regression")



############################################
### Regression + Stationary Errors Model ###
############################################

# auto.arima
library(forecast)
auto.arima(out.polynomial$residuals, d = 0) # change the fitted model


# (Preliminary) Applying GLS with ARMA(p, q) errors
n = length(data)
col.const = rep(1, n)
col.x1 = x
col.x2 = x2 

X = cbind(col.const, col.x1, col.x2)

fit.reg = Arima(data, order = c(5, 0, 1), xreg = X, include.mean = FALSE)  # change the order

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(fit.reg$residuals, type = "l"); title("Time Plot")
acf.plot = acf(fit.reg$residuals, lag = 50, plot = FALSE); plot(acf.plot, main = "SACF")
pacf.plot = pacf(fit.reg$residuals, lag = 50, plot = FALSE); plot(pacf.plot, main = "SPACF")


# Model Selection by Information Criteria
arima.fit = NULL

pmin = 5; qmin = 0 # change the minimum values
pmax = 9; qmax = 2 # change the maximum values

result.df = expand.grid(val_p = pmin:pmax, val_q = qmin:qmax, AIC = NA, AICC = NA, BIC = NA); result.df

for(i in 1:nrow(result.df)){
  
  p = result.df$val_p[i]
  q = result.df$val_q[i]
  
  arima.fit = Arima(data, order = c(p, 0, q), xreg = X, include.mean = FALSE)
  
  m = p+q+2 # p: the number of AR parameters, q: the number of MA parameters
  
  result.df$AIC[i] = -2*arima.fit$loglik + 2*m
  result.df$AICC[i] = -2*arima.fit$loglik + 2*m*n/(n-m-1)
  result.df$BIC[i] = -2*arima.fit$loglik + m*log(n)
}

result.df[which.min(result.df$AIC), ]
result.df[which.min(result.df$AICC), ]
result.df[which.min(result.df$BIC), ]

result1 = result.df


# Model Selection by Out-of-Sample Forecasting Errors
arima.fit = NULL

m = 30
N = n - m


pmin = 5; qmin = 0 # change the minimum values
pmax = 9; qmax = 2 # change the maximum values

result.df = expand.grid(val_p = pmin:pmax, val_q = qmin:qmax, err = NA); result.df

for(j in 1:nrow(result.df)){
  p = result.df$val_p[j]
  q = result.df$val_q[j]
  
  err = numeric(m)
  
  for(i in 1:m){
    train.idx = 1:(N+i-1)
    arima.fit = Arima(data[train.idx], order = c(p, 0, q), xreg = X[train.idx, ], include.mean = FALSE)
    
    X.hat = forecast(arima.fit, xreg = t(as.matrix(X[N+i, ])), h = 1)
    err[i] = (data[N+i] - X.hat$mean)^2
  }
  result.df$err[j] = mean(err)
  print(c("currently done with", j))
}

result.df[which.min(result.df$err), ]

result2 = result.df


fit.reg = Arima(data, order = c(6, 0, 0), xreg = X, include.mean = FALSE)  # change the order

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(fit.reg$residuals, type = "l"); title("Time Plot")
acf.plot = acf(fit.reg$residuals, lag = 100, plot = FALSE); plot(acf.plot, main = "SACF")
pacf.plot = pacf(fit.reg$residuals, lag = 100, plot = FALSE); plot(pacf.plot, main = "SPACF")

fit.reg = Arima(data, order = c(5, 0, 0), xreg = X, include.mean = FALSE)  # change the order

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(fit.reg$residuals, type = "l"); title("Time Plot")
acf.plot = acf(fit.reg$residuals, lag = 100, plot = FALSE); plot(acf.plot, main = "SACF")
pacf.plot = pacf(fit.reg$residuals, lag = 100, plot = FALSE); plot(pacf.plot, main = "SPACF")


# Final Regression + ARMA errors model
arma.selected =  Arima(data, order = c(6, 0, 0), xreg = X, include.mean = FALSE) # change the order

tseries::adf.test(arma.selected$residuals, k = 100)


# Diagnostics: Coefficients - Are the coefficients significantly away from zero?
2*(1-pnorm(abs(arma.selected$coef[1:6]/(sqrt(diag(arma.selected$var.coef[1:6, 1:6])))))) < 0.05 # change the slicing index

arma.selected = Arima(data, order = c(6, 0, 0), xreg = X, include.mean = FALSE) # change the fixed values

tseries::adf.test(arma.selected$residuals, k = 100)


# Diagnostics: Residual Plot - No patterns in residual plot, normal QQ Plot, SACF, SPACF?
layout(matrix(c(1, 1, 2, 3, 4, 4, 5, 5), 2, 4, byrow = TRUE))

plot(x = 1:n, y = arma.selected$residuals, type = "l")
title("Residuals - Regression")

plot(arma.selected$fitted, arma.selected$residuals ,xlab = "Fitted Values", ylab = "Residuals")
title("Residuals vs Fitted - Regression")

qqnorm(arma.selected$residuals)
qqline(arma.selected$residuals)

acf.plot = acf(arma.selected$residuals, lag = 100, plot = FALSE)
plot(acf.plot, main = "SACF of the Residuals - Regression")

pacf.plot = pacf(arma.selected$residuals, lag = 100, plot = FALSE)
plot(pacf.plot, main = "SPACF of the Residuals - Regression")


# Diagnostics: Formal Tests - Are the Residuals IID?
itsmr::test(arma.selected$residuals)


# Diagnostics: Test for Normality
shapiro.test(arma.selected$residuals)

library(nortest)
ad.test(arma.selected$residuals)
cvm.test(arma.selected$residuals)
lillie.test(arma.selected$residuals)

library(tseries)
jarque.bera.test(arma.selected$residuals)




####################
### SARIMA Model ###
####################

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(data, type = "l"); title("Time Plot") # change the lag value for seasonal differencing
acf.plot = acf(data, lag = 50, plot = FALSE); plot(acf.plot, main = "SACF") # change the lag value for seasonal differencing
pacf.plot = pacf(data, lag = 50, plot = FALSE); plot(pacf.plot, main = "SPACF") # change the lag value for seasonal differencing



layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(diff(data), type = "l"); title("Time Plot") # change the lag value for seasonal differencing
acf.plot = acf(diff(data), lag = 50, plot = FALSE); plot(acf.plot, main = "SACF") # change the lag value for seasonal differencing
pacf.plot = pacf(diff(data), lag = 50, plot = FALSE); plot(pacf.plot, main = "SPACF") # change the lag value for seasonal differencing


layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(diff(data, lag = 4), type = "l"); title("Time Plot") # change the lag value for seasonal differencing
acf.plot = acf(diff(data, lag = 4), lag = 50, plot = FALSE); plot(acf.plot, main = "SACF") # change the lag value for seasonal differencing
pacf.plot = pacf(diff(data, lag = 4), lag = 50, plot = FALSE); plot(pacf.plot, main = "SPACF") # change the lag value for seasonal differencing

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(diff(diff(data, lag = 4)), type = "l"); title("Time Plot")
acf.plot = acf(diff(diff(data, lag = 4)), lag = 50, plot = FALSE); plot(acf.plot, main = "SACF")
pacf.plot = pacf(diff(diff(data, lag = 4)), lag = 50, plot = FALSE); plot(pacf.plot, main = "SPACF")

auto.arima(data, seasonal = TRUE)


# SARIMA(p, d, q)(P, D, Q)s
# s = 7
# p = 1, d = 1, q = 0
# P = 0, D = 0, Q = 1

sarima.fit = arima(data, order = c(5, 1, 2), seasonal = list(order = c(0, 0, 0), period = 4)) # change the order and period

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(sarima.fit$residuals, type = "l"); title("Time Plot")
acf.plot = acf(sarima.fit$residuals, lag = 50, plot = FALSE); plot(acf.plot, main = "SACF")
pacf.plot = pacf(sarima.fit$residuals, lag = 50, plot = FALSE); plot(pacf.plot, main = "SPACF")



# Model Selection by Information Criteria 

sarima.fit = NULL

n = length(data)

pmin = 3; qmin = 0 # change the minimum value
pmax = 6; qmax = 2 # change the minimum value
Pmin = 0; Qmin = 0 # change the minimum value
Pmax = 2; Qmax = 2 # change the minimum value

result.df = expand.grid(val_p = pmin:pmax, val_q = qmin:qmax, val_P = Pmin:Pmax, val_Q = Qmin:Qmax, AIC = NA, AICC = NA, BIC = NA); result.df

for(i in 1:nrow(result.df)){
  skip_to_next = FALSE
  print(i)
  
  p = result.df$val_p[i]
  q = result.df$val_q[i]
  P = result.df$val_P[i]
  Q = result.df$val_Q[i]
  
  tryCatch(expr = {
    sarima.fit = arima(data, order = c(p, 1, q), seasonal = list(order = c(P, 0, Q), period = 4)) # change the number of differencing and period
    m = p+q+P+Q+2
    result.df$AIC[i] = -2*sarima.fit$loglik + 2*m
    result.df$AICC[i] = -2*sarima.fit$loglik + 2*m*n/(n-m-1)
    result.df$BIC[i] = -2*sarima.fit$loglik + m*log(n)},
    error = function(e){skip_to_next = TRUE})
  
  if(skip_to_next == TRUE){
    next
  }
}

result.df

result.df[which.min(result.df$AIC), ]
result.df[which.min(result.df$AICC), ]
result.df[which.min(result.df$BIC), ]

result3 = result.df



# Model Selection by Out-of-Sample Forecasting Errors

m = 30
N = n - m

pmin = 0; qmin = 0 # change the minimum value
pmax = 2; qmax = 2 # change the minimum value
Pmin = 0; Qmin = 0 # change the minimum value
Pmax = 2; Qmax = 2 # change the minimum value

result.df = expand.grid(val_p = pmin:pmax, val_q = qmin:qmax, val_P = Pmin:Pmax, val_Q = Qmin:Qmax, err = NA); result.df


for(j in 1:nrow(result.df)){
  skip_to_next = FALSE
  print(j)
  
  p = result.df$val_p[j]
  q = result.df$val_q[j]
  P = result.df$val_P[j]
  Q = result.df$val_Q[j]
  
  err = numeric(m)
  
  tryCatch(expr = {
    for(i in 1:m){
      train.idx = 1:(N+i-1)
      # change the number of differencing and period
      sarima.fit = arima(data[train.idx], order = c(p, 0, q), seasonal = list(order = c(P, 1, Q), period = 4), method = c("CSS-ML"))
      X.hat = forecast(sarima.fit, h = 1)$mean
      err[i] = (data[N+i] - X.hat)^2
    }
    result.df$err[j] = mean(err)},
    error = function(e){skip_to_next = TRUE})
  
  if(skip_to_next == TRUE){
    next
  }
}

result.df[which.min(result.df$err), ]
dplyr::arrange(result.df, err)
result4 = result.df


# FINAL MODEL: SARIMA
# change the order, number of differencing and period
sarima.selected = arima(data, order = c(3, 1, 1), seasonal = list(order = c(1, 0, 1), period = 4), method = c("CSS-ML"))

tseries::adf.test(sarima.selected$residuals)

# Are the coefficients significantly away from zero?
2*(1-pnorm(abs(sarima.selected$coef/(sqrt(diag(sarima.selected$var.coef)))))) < 0.05

# change the order, number of differencing, period and fixed valued
sarima.selected = arima(data,  order = c(2, 0, 3), seasonal = list(order = c(2, 1, 2), period = 4),
                        fixed = c(NA, NA, 0, 0, 0, 0, NA, 0, NA), method = c("CSS-ML"))

sarima.selected
# No patterns in residual plot, normal QQ Plot, SACF, SPACF?
par(mfrow = c(2, 2))
plot(sarima.selected$residuals, type = "l"); title("Residual Plot")
qqnorm(sarima.selected$residuals); qqline(sarima.selected$residuals)
acf.plot = acf(sarima.selected$residuals, lag = 200, plot = FALSE); plot(acf.plot, main = "SACF")
pacf.plot = pacf(sarima.selected$residuals, lag = 200, plot = FALSE); plot(pacf.plot, main = "SPACF")

# Formal tests to check if the residuals are IID noise?
itsmr::test(sarima.selected$residuals)

# Test for normality?
shapiro.test(sarima.selected$residuals)

library(nortest)
ad.test(sarima.selected$residuals)
cvm.test(sarima.selected$residuals)
lillie.test(sarima.selected$residuals)

library(tseries)
jarque.bera.test(sarima.selected$residuals)


###################
### Forecasting ###
###################

# Regression + ARMA errors model
detach("package:itsmr")
library(forecast)

n = length(data)
new.const = rep(1, 30)
new.x1 = (n+1):(n+30)
new.x2 = new.x1^2
# new.t = 1:30
# new.d = 7 # change the seasonal period
# new.f1 = 30/new.d
# new.f2 = 2*new.f1
# new.cos1 = cos(new.f1*2*pi/30*new.t)
# new.sin1 = sin(new.f1*2*pi/30*new.t)
new.X = cbind(new.const, new.x1, new.x2) # change the columns
colnames(new.X) <- c("col.const", "col.x1", "col.x2") # change the column names

par(mfrow = c(1, 1))
plot(forecast(arma.selected, xreg = new.X, h = 30))
forecast(arma.selected, xreg = new.X[1:4, ], h = 4) # change the slicing index and the value of h


# SARIMA model
par(mfrow = c(1, 1))
plot(forecast(sarima.selected, h = 30))
forecast(sarima.selected, h = 4) # change the value of h










