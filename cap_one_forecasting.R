# Forecasting Capital One stock Price
require("Quandl")
require("dplyr")
#library(Quandl)#install.packages("dplyr")#install.packages("Quandl")

# Quandl API Key
Quandl.auth("o7yyDXksn3Y9B-szcmfp")

#Quandl.search("Capital One")
#Quandl.search("COF")


cof_price_history <- Quandl("YAHOO/COF")

head(cof_price_history)
cof_price_history$WeekNumber <- as.numeric(format(cof_price_history$Date, "%U"))
head(cof_price_history)

