library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(lubridate)

#read stick-twits_sentiments generate from stock_twits_processing.R file
data <- read_csv(file = "https://raw.githubusercontent.com/goodwillyoga/E107project/master/pooja/data/stock_twits_sentiment_score.csv")[-1]
colnames(data)[3] <- "createdat"

#Convert to UTC to EST time zone 
data <- data %>% mutate(utc.time = as.POSIXct(data$createdat, tz="UTC")) %>%
                 mutate(est.time = format(utc.time, tz="America/New_York")) %>%
                 mutate(est.date = substr(as.character(format(strftime(est.time, '%m/%d/%Y'))), 2,10)) %>%
                 mutate(est.hour = paste(est.date, hour(est.time)))
#head(data$utc.time)
#head(data$est.time)
#head(data$est.hour)

#Average the sentiment score by day
avg_sentiment_score <- data %>% group_by(symbol, dayhour = est.hour) %>%
            mutate(avg_score = mean(sentiment_score)) %>%
            select(symbol, dayhour, avg_score) %>%
            unique() 
  
#View(data)
#View(avg_sentiment_score$dayhour)

#Bar plot for high-volatility and low-volatility??
data %>% filter(symbol %in% c("APPL", "YHOO", "MSFT", "TSLA", "GOOG", "FB", "EIX", "GS")) %>%
  ggplot(aes(x = sentiment_score, fill=symbol, color=symbol)) +
  geom_histogram() 

avg_sentiment_score %>% filter(symbol %in% c("APPL", "YHOO", "MSFT", "TSLA", "GOOG", "FB", "EIX", "GS")) %>%
  ggplot(aes(x = avg_score, fill=symbol, color=symbol)) +
  geom_histogram() 


#get hour from time
get_24hour <- function(x){
  ret <- ''
  splitVector<- strsplit(x,':')
  ret <- sapply(splitVector, function(z){
    if(str_count(z[2],'AM')|str_count(z[2],'am')){
      paste( z[1])
    }else{
      paste( ifelse(as.numeric(z[1]) == 12,12, as.numeric(z[1])+12 ))
    }
  })
  return(ret)
}

load("/Users/poojasingh/Documents/HE107/E107project/pulkit/yahoo-finance.RData")
#View(stocks)
avg_stocks_price <- stocks %>% 
                   mutate(dayhour = paste(lastTradeDate,get_24hour(lastTradeTime))) %>%
                   group_by(symbol, dayhour) %>%
                   mutate(close_price = mean(price)) %>%
                   select(symbol, dayhour, close_price) %>%
                   unique()
#View(avg_stocks_price$dayhour )

#Plot stock Price Vs. by day and sentiment score as the size
dat <- inner_join(avg_sentiment_score, avg_stocks_price)
#View(dat)

dat %>% filter(symbol %in% c("AAPL", "EIX", "GS")) %>%
  ggplot(aes(x = dayhour, y=close_price, size=avg_score, color=symbol, label=symbol)) + geom_point() +
  labs(title="Hourly Stock Price and Sentiment Score", x="Hour of the Day", y="Stock Price") +
  theme(axis.text.x = element_blank())


