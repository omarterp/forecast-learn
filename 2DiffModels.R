Amtrak.data <- read.csv("./Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

library("forecast")

par(mfrow = c(2, 2))

plot(ridership.ts, ylim = c(1300, 2200),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

diff.once1.ts <- diff(ridership.ts, lag = 1)

plot(diff.once1.ts, ylim = c(-400, 400),  ylab = "Ridership (Lag 1 Difference)", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

diff.once12.ts <- diff(ridership.ts, lag = 12)

plot(diff.once12.ts, ylim = c(-400, 400),  ylab = "Ridership (Lag 12 Difference)", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

diff.twice.ts <- diff(diff(ridership.ts, lag = 12), lag = 1)

plot(diff.twice.ts, ylim = c(-400, 400),  ylab = "Ridership (Twice-Differenced)", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

par(mfrow = c(1, 1))

nValid <- 36
nTrain <- length(diff.twice.ts) - nValid
train.ts <- window(diff.twice.ts, start = c(1992, 2), end = c(1992, nTrain + 1))
valid.ts <- window(diff.twice.ts, start = c(1992, nTrain + 2), end = c(1992, nTrain + 1 + nValid))
ses <- auto.arima(train.ts)
ses.pred <- forecast(ses, h = nValid)
plot(ses.pred, ylim = c(-250, 300),  ylab = "Ridership (Twice-Differenced)", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)





