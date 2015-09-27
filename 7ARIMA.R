Amtrak.data <- read.csv("/Users/jasonmerrick2/Documents/Teaching/Exec Ed/Decision Analytics/Forecasting/Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

library("forecast")

plot(ridership.ts)

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

tsdisplay(train.ts)

fitARIMA <- arima(train.ts, order = c(1,0,0))
summary(fitARIMA)
Box.test(residuals(fitARIMA), lag=24, fitdf=1, type="Ljung-Box")

residualARIMA <- arima.errors(fitARIMA)
tsdisplay(residualARIMA)

par(mfrow = c(2, 1))
forecastARIMA <- forecast(fitARIMA, level=c(80,95), h=12)
plot(forecastARIMA)

diff.train.ts <- diff(train.ts, lag = 1)

tsdisplay(diff.train.ts)

fitSARIMA <- auto.arima(train.ts)#arima(train.ts, order = c(0,1,0), seasonal=c(1,0,0))
summary(fitSARIMA)
Box.test(residuals(fitSARIMA), lag=24, fitdf=1, type="Ljung-Box")

residualSARIMA <- arima.errors(fitSARIMA)
tsdisplay(residualSARIMA)

forecastSARIMA <- forecast(fitSARIMA, level=c(80,95), h=nValid)
plot(forecastSARIMA)

par(mfrow = c(2, 1))
hist(forecastSARIMA$residuals, ylab = "Frequency", xlab = "Fit Error", bty = "l", main = "")
hist(valid.ts - forecastSARIMA$mean, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

accuracy(forecastSARIMA$mean, valid.ts)