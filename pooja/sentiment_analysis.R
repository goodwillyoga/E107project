library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

#read stick-twits_sentiments generate from stock_twits_processing.R file
data <- read_csv(file = "https://raw.githubusercontent.com/goodwillyoga/E107project/master/pooja/data/stock_twits_sentiment_score.csv")[-1]
colnames(data)[3] <- "createdat"

#Convert to UTC to EST time zone 
data <- data %>% mutate(utc.time = as.POSIXct(data$createdat, tz="UTC")) %>%
                 mutate(est.time = format(utc.time, tz="EST")) %>%
                 mutate(est.date = substr(as.character(format(strftime(est.time, '%m/%d/%Y'))), 2,10))
#head(data$est.date)

#Average the sentiment score by day
avg_sentiment_score <- data %>% group_by(symbol, day = est.date) %>%
            mutate(avg_score = mean(sentiment_score)) %>%
            select(symbol, est.date, avg_score) %>%
            unique() %>% 
            arrange(symbol, est.date)
  
#View(data)
#View(avg_sentiment_score)

#Bar plot for high-volatility and low-volatility??
data %>% filter(symbol %in% c("APPL", "YHOO", "MSFT", "TSLA", "GOOG", "FB", "EIX", "GS")) %>%
  ggplot(aes(x = sentiment_score, fill=symbol, color=symbol)) +
  geom_histogram() 

avg_sentiment_score %>% filter(symbol %in% c("APPL", "YHOO", "MSFT", "TSLA", "GOOG", "FB", "EIX", "GS")) %>%
  ggplot(aes(x = avg_score, fill=symbol, color=symbol)) +
  geom_histogram() 


load("/Users/poojasingh/Documents/HE107/E107project/pulkit/yahoo-finance.RData")
#View(stocks)
avg_stocks_price <- stocks %>% 
                   group_by(symbol, day = lastTradeDate) %>%
                   mutate(avg_price = mean(price)) %>%
                   select(symbol, day, avg_price )
head(avg_stocks_price$day)

#Plot stock Price Vs. by day and sentiment score as the size
dat <- inner_join(avg_sentiment_score, avg_stocks_price)
View(dat)

