#install.packages("rjson")
library("rjson")

msgs.df <- c()
trending_stock <- c("GILD","EIX","GS","AMZN", "RKUS","AAPL","POM","GRPN","XIV","YHOO","VA","MSFT","TSLA","BSX","NVDA","ORCL","EW","CPGX","MRK","V","BXLT","FOXA","ERIC","AVP","TWX","CMCSA","XRX","WY","GNCA","WBA","MO","MA","FOLD","TLT","SNY","RTN","UTX","LOW","MAS","GPT","RICE","IBM","KHC","CDNS","ANTM","HD","INO","OCLR","LULU","SABR","DYN","AXLL","WEN","COH","GOOG","FB","TWTR")


for(k in c(1:50))
{
  stock_twits <- function(msgs.df, stock){
    #Get raw data
    url <- paste("https://api.stocktwits.com/api/2/streams/symbol/",stock,".json", sep="")
    
    msgs <- c()
    for (i in c(1:length(url)))
    {
      raw <- readLines(url[i], warn="F")
      resp <- fromJSON(raw)
      for(j in c(1:length(resp$messages)))
      {
        msg <- unlist(resp$messages[j])
        msgs <- rbind(msgs, c(msg["id"],msg["body"], msg["created_at"], stock[i]))
      }
    }
    eval.parent(substitute( msgs.df <- unique(rbind(msgs.df, msgs))))
  }

  stock_twits(msgs.df, trending_stock)
  print(paste("ite", k))
  write.csv(msgs.df, file = "/Users/poojasingh/Documents//HE107/stock-twits-multiple.csv", row.names=FALSE, col.names=FALSE, append=TRUE)
  Sys.sleep(15*60) #sleep for 15 minutes
}

View(msgs.df)

#write.csv(msgs.df, file = "/Users/poojasingh/Documents//HE107/stock-twits-multiple.csv", row.names=FALSE, col.names=FALSE, append=TRUE)

