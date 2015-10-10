#install.packages("RCurl")
#install.packages("jsonlite")
library(stringr)
library(RCurl)
library(jsonlite)

api_key <- "82FCC78728FEFFF92589851ED225FEBF"

PET_MGFUPP12_M <- "http://api.eia.gov/series/?api_key=%API_KEY%&series_id=PET.MGFUPP12.M"

time_series_url <- str_replace(PET_MGFUPP12_M, "%API_KEY%", api_key)

json <- getURL(time_series_url, .encoding = "UTF-8")
json <- paste("{",str_sub(json, 2, str_length(json)))

jsonlite::validate(json)

str_detect(fromJSON(json), "data")