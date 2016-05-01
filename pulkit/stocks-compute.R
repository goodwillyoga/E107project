library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# Following is the list of all the stocks which we have downloaded the tweets and the stocks from Yahoo finance
tickers_symbols <- c("GILD","EIX","GS","AMZN", "RKUS","AAPL","GRPN","XIV","YHOO","VA","MSFT","TSLA","BSX","NVDA","ORCL","EW","CPGX","MRK","V","BXLT","FOXA","ERIC","AVP","TWX","CMCSA","XRX","WY","GNCA","WBA","MO","MA","FOLD","TLT","SNY","RTN","UTX","LOW","MAS","GPT","RICE","IBM","KHC","CDNS","ANTM","HD","INO","OCLR","LULU","SABR","DYN","AXLL","WEN","COH","GOOG","FB","TWTR","XOM","PSX","VLO","PGR","CINF","FAF","JBLU","DAL","HA","ACN","INFY","CTSH")
# Sectors associate with each stock
sectors <- c("Healthcare","Utilities","Financial","Services","Technology","Consumer Goods","Technology","Financial","Technology","Services","Technology","Consumer Goods","Healthcare","Technology","Technology","Healthcare","Basic Materials","Healthcare","Financial","Healthcare","Services","Telecommunications","Consumer Goods","Services","Services","Technology","Industrial Goods","Healthcare","Services","Consumer Goods","Financial","Healthcare",
             "Financial","Healthcare","Industrial Goods","Industrial Goods","Services","Industrial Goods","Financial","Basic Materials","Technology","Consumer Goods","Technology","Healthcare","Services","Healthcare","Technology","Consumer Goods","Technology","Utilities","Basic Materials","Services","Consumer Goods","Technology","Technology","Technology","Basic Materials","Basic Materials","Basic Materials","Financial","Financial","Financial",
             "Services-Airlines","Services-Airlines","Services-Airlines","Technology","Technology","Technology")
# Make a dataframe for symbols and the corresponding sectors
ticker_sector <- data.frame(symbol = tickers_symbols, sector = sectors)

# Load the stocks datset into memory
load(url("https://github.com/goodwillyoga/E107project/raw/master/pulkit/yahoo-finance.RData"))

#conver the date to proper format
convert_24Time <- function(x){
  ret <- ''
  splitVector<- strsplit(x,':')
  ret <- sapply(splitVector, function(z){
    if(str_count(z[2],'AM')){
      paste( z[1],':', str_replace(z[2],'AM','') ,sep = '')
    }else{
      paste( ifelse(as.numeric(z[1]) == 12,12, as.numeric(z[1])+12 ),':', str_replace(z[2],'PM','') ,sep = '')
    }
  })
  return(ret)
}

#Function to normalize the values
normalize<-function(m){
  (m - mean(m))/sd(m)
}

#Function to find %age change between consecutive values
pcchange=function(x,lag=1) {
  c(diff(x,lag))/x
}

# Data obtained from Yahoo finance is in EST timezone.
# The date we recieve from yahoo finance is of the form 2:54PM, let us convert it into 14:54 and take away the PM part
stocks_est<- stocks %>% 
  mutate(date_time = paste(lastTradeDate,convert_24Time(lastTradeTime))) %>%
  inner_join(ticker_sector)

# Now let us use lubridate and convert to date_time
stocks_est <- stocks_est %>% mutate(date_timelb = mdy_hm(date_time), daysHigh = as.numeric(daysHigh), daysLow = as.numeric(daysLow), open = as.numeric(open), prvClose= as.numeric(prvClose))
# The trading hours vary from 9:30 a.m. to 4:00 p.m EST. After the tradinng hours when we query the api it keeps on returning the values for that day. 
nrow(stocks_est)
# Let us get the distict values for all the stocks
stocks_est <- distinct(stocks_est)
nrow(stocks_est)

# Let us make a dataset of the dailyStock data, this dataset store the closingprice and the volume for each day
dailyStockData <- stocks_est %>% 
  mutate(day = mdy(lastTradeDate)) %>% 
  group_by(day,symbol) %>% 
  filter(volume == max(volume)) %>% 
  mutate(prcChange = open - price) %>% mutate(absPrcChange = abs(prcChange))

# Create a dataset with hourly price change
hourly_avg_stockPriceChng <- stocks_est %>% 
  mutate(day_hr = make_datetime(year = year(date_timelb), month = month(date_timelb), day = day(date_timelb), hour=hour(date_timelb)), day = as.Date(date_timelb)) %>% 
  group_by(symbol,day,day_hr) %>% summarise(avgPrice = mean(price)) %>% 
  group_by(symbol, day) %>% mutate(n=n()) %>%  filter(n>1) %>% arrange(day_hr) %>% 
  mutate(prcChange = pcchange(avgPrice)) %>% ungroup()

# Above dataset contains the hourly average price, and hourly price change, let us add to it the hourly volume as well
hourly_volumeChng <- stocks_est %>%
  mutate(day_hr = make_datetime(year = year(date_timelb), month = month(date_timelb), day = day(date_timelb), hour=hour(date_timelb)), day = as.Date(date_timelb)) %>% 
  group_by(symbol,day,day_hr) %>% filter(volume == max(volume)) %>%
  group_by(symbol, day) %>% mutate(n=n()) %>% filter(n>1) %>% arrange(day_hr) %>% 
  mutate(hourlyVolumeChng = pcchange(volume)) %>% ungroup()

hist(hourly_avg_stockPriceChng$prcChange)
# Make a hourly dataset
hourly_stockData <- inner_join(hourly_avg_stockPriceChng,hourly_volumeChng) %>% select(symbol,price,volume,sector,date_timelb,day_hr,day,hourlyVolumeChng,prcChange,avgPrice)

save(stocks_est, hourly_stockData, dailyStockData,  file = "/code/CSCIE-107/E107project/pulkit/processed-stocks.RData")


