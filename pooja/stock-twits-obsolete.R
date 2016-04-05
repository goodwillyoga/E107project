setInternet2(TRUE) 
Sys.setenv(http_proxy="http://10.173.10.100:8080")
options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")


#install.packages("rjson")
library("rjson")

url <- "https://api.stocktwits.com/api/2/streams/symbol/AAPL.json"
#browseURL(url) opens a browser, prompts to viewthe json response 

msgs.df <- c()

for(d in 1 : 10)
{
  #Get raw data
  raw <- readLines(url, warn="F")
  resp <- fromJSON(raw)
  length(resp$messages)
  for(i in c(1:length(resp$messages)))
  {
    msg <- unlist(resp$messages[i])
    msgs.df <- rbind(msgs.df, c(msg["id"],msg["body"], msg["created_at"]))
  }
  paste("ran ite:",d)
  Sys.sleep(15*60) #sleep for 15 minutes

}

View(msgs.df)

write.csv(msgs.df, file = "C:\\Users\\singhp\\Documents\\H107\\stock-twits2.csv", row.names=FALSE, col.names=FALSE, append=TRUE)

# resp$messages[1][[1]]$body #id=0
# resp$messages[10][[1]]$body #id=9
# OR same results 
# resp$messages[[1]]$body
# resp$messages[[10]]$body
