Amtrak.data <- read.csv("./Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

library("forecast")

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

hw <- ets(train.ts, model = "MMA", restrict = FALSE)
plot(hw)
str(hw)

ESOpt <- ets(train.ts)
plot(ESOpt)
ESOpt

par(mfrow = c(2, 1))

hw.pred <- forecast(hw, h = nValid, level = 0)
plot(hw.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(hw.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

ESOpt.pred <- forecast(ESOpt, h = nValid, level = 0)
plot(ESOpt.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ESOpt.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

accuracy(hw.pred$mean, valid.ts)
accuracy(ESOpt.pred$mean, valid.ts)

