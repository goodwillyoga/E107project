library(readr)
library(dplyr)
# the date here are as per the EST, New York timezone

symbols <- c("GILD","EIX","GS","AMZN", "RKUS","AAPL","GRPN","XIV","YHOO","VA","MSFT","TSLA","BSX","NVDA","ORCL","EW","CPGX","MRK","V","BXLT","FOXA","ERIC","AVP","TWX","CMCSA","XRX","WY","GNCA","WBA","MO","MA","FOLD","TLT","SNY","RTN","UTX","LOW","MAS","GPT","RICE","IBM","KHC","CDNS","ANTM","HD","INO","OCLR","LULU","SABR","DYN","AXLL","WEN","COH","GOOG","FB","TWTR","XOM","PSX","VLO","PGR","CINF","FAF","JBLU","DAL","HA","ACN","INFY","CTSH")
example_url <- "http://finance.yahoo.com/d/quotes.csv"
parameter_fetch <- "&f=sl1d1t1hgvkjop"
url <- paste(example_url,"?s=",paste(symbols,collapse="%2C"),parameter_fetch, "&e=.csv",sep = "")
for(i in 1:50000000){
  tryCatch({
    print("Running again ")
    prices <-read_csv(url,col_names = FALSE)
    write_csv(prices,"/code/tot/Info/twitter-data/unprocessesed/stocks.csv",append = TRUE)
  }, warning = function(war) {
    print(paste("Warning:  ",war))
  }, error = function(err) {
    print(paste("ERROR:  ",err))
  })
  Sys.sleep(15*60)
}
#tmp3 <- dailyStockData %>% select(symbol,lastTradeDate, daysHigh, daysLow, yearHigh, yearLow, open)
#write_csv(tmp3,"/code/tot/Info/twitter-data/stocks-higlow-open-alldays.csv")