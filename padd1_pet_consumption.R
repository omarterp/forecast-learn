#install.packages("RCurl")
#install.packages("jsonlite")
library(stringr)
library(RCurl)
library(jsonlite)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(reshape2)


#install_github('sinhrks/ggfortify')
library(ggfortify)

a <- require(test, quietly = TRUE, warn.conflicts = FALSE)

install.packages("devtools")
library(devtools)
build_github_devtools()



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


# Create Time Series Object
fuel_consumption_ts <- ts(time_series_df$consumption, start = c(1981,1), end = c(2015, 7), freq = 12)
ts.plot(fuel_consumption_ts)


ts <- as.zoo(fuel_consumption_ts)

p <- ggplot(ts)
p + geom_point()

as.integer(nrow(time_series_df)/12)

dim(time_series_df)
dimnames(time_series_df)
415 %% 12

#rows
matrix_rows <- ((nrow(time_series_df))/12) + 1
nrow(time_series_df)/matrix_rows

matrix(time_series_df$consumption, nrow = matrix_rows, ncol = 12, byrow = TRUE)


p <- ggplot(fuel_consumption_ts, aes(x = "consumption", y = "month_year"))
p + geom_line()


autoplot(fuel_consumption_ts)


library(ggplot2)
## Read in data, available from:
#www.standardandpoors.com/indices/sp-case-shiller-home-price-indices/en/us/?indexId=spusa-cashpidff--p-us----

melt(time_series_df, id.vars = "month_year")

mdf=melt(dat,id.vars="YEAR")
mdf$Date=as.Date(paste("01-",mdf$YEAR,sep=""),"%d-%b-%y")
names(mdf)=c("MonthYear","City","IndexValue","Date")
ggplot(data=mdf,aes(x=Date,y=IndexValue)) + geom_line(aes(color=City),size=1.25) +
  scale_x_date("Year", minor="years") + scale_y_continuous("Case Schiller Index")
sm=subset(mdf,City %in% c('NY.New.York','FL.Miami','CA.Los Angeles','MI.Detroit',
                          'TX.Dallas','IL.Chicago','DC.Washington'))
sm$City=droplevels(sm$City)
ggplot(data=sm,aes(x=Date,y=IndexValue)) + geom_line(aes(color=City),size=1.5) +
  scale_x_date("Year", minor="years") + scale_y_continuous("Case Schiller Index")
