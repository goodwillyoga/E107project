library("rjson")

price.df <- c()
trending_stock <- c("GILD","EIX","GS","AMZN", "RKUS","AAPL","POM","GRPN","XIV","YHOO","VA","MSFT","TSLA","BSX","NVDA","ORCL","EW","CPGX","MRK","V","BXLT","FOXA","ERIC","AVP","TWX","CMCSA","XRX","WY","GNCA","WBA","MO","MA","FOLD","TLT","SNY","RTN","UTX","LOW","MAS","GPT","RICE","IBM","KHC","CDNS","ANTM","HD","INO","OCLR","LULU","SABR","DYN","AXLL","WEN","COH","GOOG","FB","TWTR")
colnames=TRUE
for(k in c(1:40)) 
{
  stock_price <- function(price.df, stock){
    #Get raw data
    url <- paste("http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(%22",stock, "%22)&env=http%3A%2F%2Fdatatables.org%2Falltables.env&format=json", sep="")
    print(url)
    price <- c()
    for (i in c(1:length(url)))
    {
      raw <- readLines(url[i], warn="F")
      resp <- fromJSON(raw)
      for(j in c(1:length(resp$query$results)))
      {
        #msg <- unlist(resp$messages[j])
        price <- rbind(price, c(resp$query$created,resp$query$results[j]$quote$symbol,resp$query$results[j]$quote$DaysLow,resp$query$results[j]$quote$DaysHigh,
                                resp$query$results[j]$quote$Open,resp$query$results[j]$quote$PreviousClose,resp$query$results[j]$quote$PercentChange,
                                resp$query$results[j]$quote$ChangeinPercent))
      }
    }
    eval.parent(substitute( price.df <- unique(rbind(price.df, price))))
  }
  
  stock_price(price.df, trending_stock)
  if(colnames){
     colnames(price.df) <- c("created", "symbol", "dayslow", "dayshigh", "open", "previousclose", "percentchange", "changeinpercent")
  }
  colnames = FALSE
  print(paste("ite", k))
  write.csv(price.df, file = "/Users/poojasingh/Documents//HE107/stock-price-multiple.csv", row.names=FALSE, col.names=FALSE, append=TRUE)
  Sys.sleep(15*60) #sleep for 15 minutes
}

#View(price.df)

#readLines("http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(%22YHOO%22%2C%22AAPL%22%2C%22GOOG%22%2C%22MSFT%22)&env=http%3A%2F%2Fdatatables.org%2Falltables.env&format=json", warn="F")

