Call.data <- read.csv("/Users/jasonmerrick2/Documents/Teaching/Exec Ed/Decision Analytics/Forecasting/Call Volumes vs Nos Accounts.csv")

CVProd1.ts <- ts(Call.data$CallVolProd1, 
                 start = c(2009,45), end = c(2011, 12), freq = 52)
Accts1Week.ts <- ts(Call.data$NosAccts1WeeksOld, 
                    start = c(2009,45), end = c(2011, 12), freq = 52)

library("forecast")

tsdisplay(CVProd1.ts)

tsdisplay(Accts1Week.ts)

par(mfrow = c(2, 1))

plot(CVProd1.ts, xlab = "Time", ylab = "Call Volume Product 1", bty = "l")
plot(Accts1Week.ts, xlab = "Time", ylab = "Nos. New Accounts", bty = "l")

par(mfrow = c(2, 1))

plot(Call.data$CallVolProd1 ~ Call.data$NosAccts1WeeksOld, xlab="Nos. New Accounts",
     ylab="Product 1 Call Volume)", data = Call.data)
fit1 <- tslm(CVProd1.ts ~ Accts1Week.ts)
abline(fit1)
summary(fit1)

res <- ts(resid(fit1),s=2009+45/52,f=4)
plot(res, xlab = "Time", ylab = "Residuals from Linear Regression", bty = "l")

tsdisplay(res)

par(mfrow = c(2, 1))

fitwithoutX <- auto.arima(Call.data$CallVolProd1)
CVforecastwithoutX <- forecast(fitwithoutX, level=c(80,95), h=4)
plot(CVforecastwithoutX)

LAG <- function(List,periods){c(rep(NA,periods),List[1:(length(List)-periods)])}

Call.data$NosAccts1WeeksOldLag4 <- LAG(Call.data$NosAccts1WeeksOld,4)
NosAccts1WeeksOldLag4.ts <- ts(Call.data$NosAccts1WeeksOldLag4, start = c(2009,45), end = c(2011, 12), freq = 52)

fitwithX <- auto.arima(Call.data$CallVolProd1, xreg=Call.data$NosAccts1WeeksOldLag4)
n <- length(Call.data$NosAccts1WeeksOld)
xforecast <- c(Call.data$NosAccts1WeeksOld[(n-3):n])
CVforecastwithX <- forecast(fitwithX, xreg = xforecast, level=c(80,95))
plot(CVforecastwithX)

summary(fitwithX)
summary(fitwithoutX)

tsdisplay(arima.errors(fitwithX), main="ARIMA errors")
tsdisplay(arima.errors(fitwithoutX), main="ARIMA errors")

?auto.arima