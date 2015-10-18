library("forecast")
library(MASS)
library(ggplot2)
library(zoo)
gas.data <-read.csv("ECPADD.csv")

gas.ts <-ts(gas.data$East.Coast..PADD.1..Product.Supplied.of.Finished.Motor.Gasoline.Thousand.Barrels.per.Day,
            start = c(1981, 1), end = c(2015, 6),freq = 12)

plot(gas.ts)
?plot
?grid
plot(1:3)
grid(6, 5, lwd = 2)

nValid <-12
nTrain <- length(gas.ts)-nValid
gastrain.ts <- window(gas.ts, start = c(1981, 1), end = c(1981, nTrain))
gasvalid.ts <- window(gas.ts, start = c(1981, nTrain +1), end= c(1981, length(gas.ts)))
___________


tsdisplay(gastrain.ts)

# #Regression Model
# ?lm
# 
# gas.reg <- lm(gas.data$East.Coast..PADD.1..Product.Supplied.of.Finished.Motor.Gasoline.Thousand.Barrels.per.Day ~ gas.data$September.11th + gas.data$Prius.Hatchback +gas.data$Gulf.War)
# 
# 
# ?stepAIC
# 
# plot(gas.reg)
# summary(gas.reg)





#Linear Model Exploration
gastrain.lm <- tslm(gastrain.ts ~trend)
par(bg= "lightblue")

plot(gastrain.ts, xlab = "Time", ylab = "Gas PADD", ylim = c(2000, 3500), bty = "l",lwd = 1 )
grid(8, 8, col = "grey", lwd= .5, lty = 1 )
lines(gastrain.lm$fitted, lwd=2)



summary(gastrain.lm)


#Differencing



#Put X value on this graph
gasdiff.once1.ts <- diff(gas.ts, lag = 1)
plot(gasdiff.once1.ts, ylim = c(-450, 400),  ylab = "Gas Production", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1981,2015), main = "", lty = 1)
axis(1, at = seq(1981, 2015, 1), labels = format(seq(1981, 2015, 1)))

tsdisplay(gasdiff.once1.ts)
summary(gasdiff.once1.ts)
#Difference with lag 12 also


# Arima 
tsdisplay(gastrain.ts)
gas.fitARIMA <- arima(gastrain.ts, order = c(3,1,1), 
                  seasonal = list(order=c(0,1,1),period = 12),
                  include.mean=FALSE)

summary(gas.fitARIMA)
tsdisplay(gas.fitARIMA$residuals)
Box.test(residuals(gas.fitARIMA), lag=24, fitdf=1, type="Ljung-Box")

plot(gas.fitARIMA$residuals)
gas.residualARIMA <- arima.errors(gas.fitARIMA)
gas.residualARIMA
par(mfrow = c(1,2))
plot(gas.residualARIMA)
plot(gastrain.ts)

gastrain.ts

gas.resARIMA.fit <- arima(gas.residualARIMA, order = c(1,0,0))
summary(gas.resARIMA.fit)

gas.resARIMA.forecast <-forecast(gas.resARIMA.fit, level=c(80,95), h=12)
plot(gas.resARIMA.forecast)


gas.forecastARIMA <- forecast(gas.fitARIMA, level=c(80,95), h=nValid)
plot(gas.forecastARIMA)

#naiveforecast
gas.naive <- naive(gastrain.ts)
plot(gas.naive)
summary(gas.naive)
gas.naive.forecast <- forecast(gas.naive, level=c(80,95), h=nValid)
plot(gas.naive.forecast)

#Seasonal 
gas.snaive <- snaive(gastrain.ts)
plot(gas.snaive)
plot(gas.snaive$residuals)
summary(gas.snaive)
gas.snaive.forecast <- forecast(gas.snaive, level=c(80,95), h=nValid)
plot(gas.snaive.forecast)




# Autoarima

gas.fitSARIMA <-auto.arima(gastrain.ts)
summary(gas.fitSARIMA)

Box.test(residuals(gas.fitSARIMA), lag=24, fitdf=1, type="Ljung-Box")

gas.residualSARIMA <- arima.errors(gas.fitSARIMA)

tsdisplay(gas.fitSARIMA$residuals)


#Forecasting the ARIMA Models

gas.forecastSARIMA <- forecast(gas.fitSARIMA, level=c(80,95), h=nValid)
plot(gas.forecastSARIMA, grid(nx=NULL))




par(mfrow = c(1, 1))
hist(gas.forecastSARIMA$residuals, ylab = "Frequency", xlab = "Fit Error", bty = "l", main = "")
hist(gas.forecastARIMA$residuals, ylab = "Frequency", xlab = "Fit Error", bty = "l", main = "")

hist(gasvalid.ts - gas.forecastSARIMA$mean, ylab = "Frequency SARIMA", xlab = "Forecast Error", bty = "l", main = "")
hist(gasvalid.ts - gas.forecastARIMA$mean, ylab = "Frequency ARMIA", xlab = "Forecast Error", bty = "l", main = "")

#par(mfrow = c(2, 1))
#plot(gas.forecastSARIMA$mean)
#plot(gasvalid.ts)

par(mfrow = c(2, 1))
plot(gas.forecastARIMA$mean)
plot(gasvalid.ts)


par(mfrow = c(2, 1))
plot(gas.naive.forecast$mean)
plot(gasvalid.ts)


par(mfrow = c(2, 1))
plot(gas.snaive.forecast$mean)
plot(gasvalid.ts)


par(mfrow = c(2, 1))
plot(gas.forecastSARIMA$mean)
plot(gasvalid.ts)


accuracy(gas.forecastSARIMA$mean, gasvalid.ts)
accuracy(gas.forecastARIMA$mean, gasvalid.ts)
accuracy(gas.naive.forecast$mean, gasvalid.ts)
accuracy(gas.snaive.forecast$mean, gasvalid.ts)
