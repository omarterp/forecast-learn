Amtrak.data <- read.csv("./Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

library("forecast")

fit1 <- decompose(ridership.ts, type="additive")
plot(fit1)

fit2 <- stl(ridership.ts, t.window = 12, s.window=24, robust=TRUE)
plot(fit2)

forecast2 <- forecast(fit2, h=26)
plot(forecast2)
