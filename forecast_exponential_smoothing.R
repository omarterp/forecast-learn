detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

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
         "zoo", "reshape2", "jsonlite", "devtools", "scales", "forecast", 
         "wesanderson", "RColorBrewer")

lapply(pkg, handle_package)

#detach_package("gridExtra", TRUE)


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
    ggtitle("Product Supplied of Finished Gasoline") +
    xlab("Month-Year") + 
    ylab("Thousand Barrels per Day") +
    scale_x_date(labels = date_format("%b-%y"), breaks = "3 years") +
    geom_smooth()

# Does the variation increase as the level increases - By Year
bp <- ggplot(time_series_df, aes(x = year , y = consumption))

bp + geom_boxplot()

# Bimodal Histogram
bm <- ggplot(time_series_df, aes(x = consumption))
bm + geom_histogram() +
  ggtitle("Product Supplied of Finished Gasoline") +  
  ylab("frequency") +
  xlab("Thousand Barrels per Day")




time_series_df_1 <- time_series_df %>%
                      filter(year < 2000)

# By Month - Bi Modal
bp <- ggplot(time_series_df_1, aes(x = month , y = consumption))

bp + geom_boxplot()

time_series_df_2 <- time_series_df %>%
                      filter(year >= 2000)

# By Month - Bi Modal
bp <- ggplot(time_series_df_2, aes(x = month , y = consumption))

bp + geom_boxplot()



# Data Partitioning
nValid <- 12
prediction_multiplier <- 2
prediction_periods <- nValid * prediction_multiplier

nTrain <- length(fuel_consumption_ts) - nValid
train.ts <- window(fuel_consumption_ts, start = c(1981, 1), end = c(1981, nTrain))
valid.ts <- window(fuel_consumption_ts, start = c(1981, nTrain + 1), end = c(1981, nTrain + nValid))


# Models

par(mfrow = c(2, 1))

# Simple Exponential Smoothing
ses <- ses(train.ts)
ses_pred <- forecast(ses, h = prediction_periods,  level = c(80,95))

plot(ses_pred, ylim = c(1800, 3500),  ylab = "Barrels (Thousands)", 
     xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1981,2015.75), main = "", flty = 2)

axis(1, at = seq(1981, 2015, 1), labels = format(seq(1981, 2015, 1)))
lines(ses_pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)


# Holt's Linear with trend - No Alpha & Beta
holt_m <- holt(train.ts)
h_pred <- forecast(holt_m, h = prediction_periods, level = c(80,95))

plot(h_pred, ylim = c(1800, 3500),  ylab = "Barrels (Thousands)", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1981,2015.75), main = "", flty = 2)
axis(1, at = seq(1981, 2015, 1), labels = format(seq(1981, 2015, 1)))
lines(h_pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)


# Holt's Linear Trend with alpha and no model restrictions
hw <- ets(train.ts, model = "MMA", alpha = 0.8, restrict = FALSE)
hw_pred <- forecast(hw, h = prediction_periods, level = c(80,95))

plot(hw_pred, ylim = c(1800, 3500),  ylab = "Barrels (Thousands)", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1981,2015.75), main = "", flty = 2)
axis(1, at = seq(1981, 2015, 1), labels = format(seq(1981, 2015, 1)))
lines(hw_pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

# Exponential Smoothing Automated
es_opt <- ets(train.ts)
es_opt_pred <- forecast(es_opt, h = prediction_periods, level = c(80,95))

plot(es_opt_pred, ylim = c(1800, 3500),  ylab = "Barrels (Thousands)", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1981,2015.75), main = "", flty = 2)
axis(1, at = seq(1981, 2015, 1), labels = format(seq(1981, 2015, 1)))
lines(es_opt_pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)


# Accuracy
accuracy(ses_pred$mean, valid.ts)
accuracy(h_pred$mean, valid.ts)
accuracy(hw_pred$mean, valid.ts)
accuracy(es_opt_pred$mean, valid.ts)



# prepare residuals
ses_residuals <- fortify(ses_pred$residuals)
ses_residuals$model <- "ses"
names(ses_residuals) <- c("time","residual", "model")

h_residuals <- fortify(h_pred$residuals)
h_residuals$model <- "holt's"
names(h_residuals) <- c("time","residual","model")

hw_residuals <- fortify(hw_pred$residuals)
hw_residuals$model <- "holt's winter"
names(hw_residuals) <- c("time","residual","model")

es_opt_residuals <- fortify(es_opt_pred$residuals)
es_opt_residuals$model <- "es optimized"
names(es_opt_residuals) <- c("time","residual","model")

# Create list of all residuals to plot
residuals <- list(ses_residuals, h_residuals, hw_residuals, es_opt_residuals)

residuals <- bind_rows( residuals )


lapply(dev.list(),dev.off)

# Plot Residuals

# Holt's Winter
#p <- ggplot(hw_residuals, aes(x = time, y = residual))
#p + geom_line(aes(colour = residual ), size = 1)



# ses and es optimization - https://github.com/karthik/wesanderson
pal <- wes_palette("Cavalcanti", type = "continuous")

p <- ggplot(residuals, aes(x = model, y = residual, fill = model))
p +  geom_boxplot() + scale_fill_manual(values = wes_palette("Royal1", 4)) +
  xlab("") +
  ggtitle("Model Comparison by Forecast Residuals")
  
  #wes_palette()
  
  
  #scale_colour_brewer(palette = "Set1")aes(colour = residual)) + 
  #geom_line(data = es_opt_residuals, aes(x = time, y = residual), colour = "blue") +
  #scale_fill_brewer()
  #scale_colour_gradient(limits=c(-300, 300), low="red", high="gray", space="Lab")

#wes_palette("Moonrise1")
#boxplot(ses_residuals, col = brewer.pal(8, "Set3"))

#p <- ggplot(ses_residuals, aes(x = time, y = residual))
#p +  geom_line(aes(colour = residual)) + 
#     geom_line(data = es_opt_residuals, aes(x = time, y = residual), colour = "#CC0033") +
#     geom_line(data = h_residuals, aes(x = time, y = residual), colour = "gray")


#RColorBrewer::display.brewer.all()
