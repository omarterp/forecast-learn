# load dependencies
handle_package <- function(x) {
  a <- require(x, character.only = TRUE)
  if(!a) {
    install.packages(x)
    require(x, character.only = TRUE)
  }
}

pkg <- c("ggplot2","dplyr","R.utils","fdrtool","caret","randomForest","pROC", 
         "readr", "stringr", "corrgram", "corrplot", "aod", "rvest", "ggfortify",
         "zoo", "reshape2", "jsonlite", "devtools", "scales", "forecast")

lapply(pkg, handle_package)


# load time series data - problems(time_series)
time_series <- read_csv("./padd1_time_series.csv", skip = 5, col_names = FALSE)
names(time_series) <- c("month_year", "consumption")

# Minor data prep - dplyr friendly object; turning id into an integer to sort
time_series_df <- tbl_df(time_series)
time_series_df$id <- as.integer(row.names(time_series_df))
time_series_df <- arrange(time_series_df, desc(id))

# Convert month_year to Date type - facilitate graphing with ggplot
time_series_df$month_year_conv <- as.Date(str_replace(time_series_df$month_year, " ", "01"), "%b%d%Y")

# Extract month, month_name and year from month_year_conv
time_series_df$month <- factor(format(time_series_df$month_year_conv, "%m"), ordered = TRUE)
time_series_df$year <- factor(format(time_series_df$month_year_conv, "%Y"), ordered = TRUE)



###par(mfrow = c(2,2)) 
###lapply(dev.list(), dev.off)


# Create time series object
# Create Time Series Object
fuel_consumption_ts <- ts(time_series_df$consumption, start = c(1981,1), end = c(2015, 7), freq = 12)

# Time Series Plot
p <- ggplot(time_series_df, aes(x = month_year_conv , y = consumption))

p + geom_line() +
    ggtitle("Fuel Consumption Time Series") +
    xlab("Month-Year") + 
    ylab("Thousands Barrels per Day") +
    scale_x_date(labels = date_format("%b-%y"), breaks = "3 years") +
    geom_smooth()



# Does the variation increase as the level increases - By Year
bp <- ggplot(time_series_df, aes(x = year , y = consumption))

bp + geom_boxplot()

# By Month - Bi Modal
bp <- ggplot(time_series_df, aes(x = month , y = consumption))

bp + geom_boxplot()


# Data Partitioning
nValid <- 18
nTrain <- length(fuel_consumption_ts) - nValid
train.ts <- window(fuel_consumption_ts, start = c(1981, 1), end = c(1981, nTrain))
valid.ts <- window(fuel_consumption_ts, start = c(1981, nTrain + 1), end = c(1981, nTrain + nValid))

# Models
hw <- ets(train.ts, model = "MMA", restrict = FALSE)
plot(hw)
str(hw)

ESOpt <- ets(train.ts)
plot(ESOpt)
ESOpt


par(mfrow = c(2, 1))

hw.pred <- forecast(hw, h = nValid, level = 0)
plot(hw.pred, ylim = c(1800, 3300),  ylab = "Barrels (Thousands)", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1981,2015.75), main = "", flty = 2)
axis(1, at = seq(1981, 2015, 1), labels = format(seq(1981, 2015, 1)))
lines(hw.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

ESOpt.pred <- forecast(ESOpt, h = nValid, level = 0)
plot(ESOpt.pred, ylim = c(1800, 3300),  ylab = "Barrels (Thousands)", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1981,2015.75), main = "", flty = 2)
axis(1, at = seq(1981, 2015, 1), labels = format(seq(1981, 2015, 1)))
lines(hw.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

accuracy(hw.pred$mean, valid.ts)
accuracy(ESOpt.pred$mean, valid.ts)

