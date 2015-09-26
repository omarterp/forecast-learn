#Auto Arima fit without any x variables
#This shows you how to do get the accuracy report from a time series forecast rather than a linear model (as we did in class).
wd <- "C:/tmp/forecasting"
setwd(wd)

#file_path <- "D:/Google Drive/Graduate Studies/VCU/trmFall2015/Forecasting/data/Call Volumes vs Nos Accounts.csv"

file_path <- "./Call Volumes vs Nos Accounts.csv"

Call.data <- read.csv(file_path)

nrow(Call.data)
#Call.data$CallVolProd1  
?ts

CVProd1.ts <- ts(Call.data$CallVolProd1, start = c(2009,45), freq = 52) #end = c(2011, 13), freq = 52)


Accts1Week.ts <- ts(Call.data$NosAccts1WeeksOld,  start = c(2009,45), end = c(2011, 12), freq = 52)


library("forecast")
install.packages("TTR")
library("TTR")

n <- length(CVProd1.ts)

plot.ts(CVProd1.ts)

CVProd1.ts.SMA <- SMA( CVProd1.ts, n = 6 )
plot.ts( CVProd1.ts.SMA )

decompose( CVProd1.ts)

Call.data$CallVolProd1

na.omit(CVProd1.ts.SMA)

#73 periods


cvprod1_ts_components <- decompose(CVProd1.ts)



(names(Call.data))






nValid <- 26L


nTrain <- n - nValid


trainY.ts <- window(CVProd1.ts, start = c(2009, 45), end = c(2009, 45 + nTrain - 1))


validY.ts <- window(CVProd1.ts, start = c(2009, 45 + nTrain), end = c(2009, 45 + n - 1))


fitwithoutX <- auto.arima(trainY.ts)

ForecastwithoutX <- forecast(fitwithoutX, level=c(80,95), h=nValid)

accuracy(ForecastwithoutX$mean, validY.ts)

?auto.arima







'
Second, you need to think carefully about the test and validation series for the call volume time series
(your Y) and the number of account time series (your Xs).

You are saving the last 26 time periods for validation, but you also need to consider that you are 
forecasting 4 time periods ahead. Here is the code for setting up the training and stepAICstepAICstepAIC?
validation time series for the Y and one X:
'

lag = 4


trainY.ts <- window(CVProd1.ts, start = c(2009, 45 + lag), end = c(2009, 45 + nTrain - 1))


validY.ts <- window(CVProd1.ts, start = c(2009, 45 + nTrain), end = c(2009, 45 + n - 1))


trainX.ts <- window(Accts1Week.ts, start = c(2009, 45), end = c(2009, 45 + nTrain - lag - 1))


validX.ts <- window(Accts1Week.ts, start = c(2009, 45 + nTrain - lag), end = c(2009, 45 + n - lag - 1))


