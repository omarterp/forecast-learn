#install.packages("RCurl")
#install.packages("jsonlite")
library(stringr)
library(RCurl)
library(jsonlite)
library(readr)
library(dplyr)
a <- require(test, quietly = TRUE, warn.conflicts = FALSE)

#http://www.eia.gov/beta/api/qb.cfm?sdid=PET.MGFUPP12.M
#East_Coast_(PADD_1)_Product_Supplied_of_Finished_Motor_Gasoline_Monthly
#padd1_time_series.csv

setwd("C:/R/projects/forecast-learn/")

api_key <- "82FCC78728FEFFF92589851ED225FEBF"

PET_MGFUPP12_M <- "http://api.eia.gov/series/?api_key=%API_KEY%&series_id=PET.MGFUPP12.M"

time_series_url <- str_replace(PET_MGFUPP12_M, "%API_KEY%", api_key)

json <- getURL(time_series_url, .encoding = "UTF-8")
json <- paste("{",str_sub(json, 2, str_length(json)))

jsonlite::validate(json)

str_detect(fromJSON(json), "data")


time_series <- read_csv("./padd1_time_series.csv", skip = 5, col_names = FALSE)
problems(time_series)
ncol(time_series)
names(time_series) <- c("month_year", "consumption")

time_series_df <- tbl_df(time_series)

time_series_df$id <- as.integer(row.names(time_series_df))


time_series_df <- arrange(time_series_df, desc(id))

fuel_consumption_ts <- ts(time_series_df$consumption, start = c(1981,1), end = c(2015, 7), freq = 12)
ts.plot(fuel_consumption_ts)
