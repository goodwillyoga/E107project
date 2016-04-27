library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

#read stick-twits_sentiments generate from stock_twits_processing.R file
data <- read_csv(file = "https://raw.githubusercontent.com/goodwillyoga/E107project/master/pooja/data/stock_twits_sentiment_score.csv")[-1]
colnames(data)[3] <- "createdat"

data <- data %>% mutate(utc.date = as.POSIXct(data$createdat, tz="UTC")) %>%
                 mutate(est.date = format(utc.date, tz="EST"))
View(data)

#Bar plot for high-volatility and low-volatility??
data %>% filter(symbol %in% c("APPL", "YHOO", "MSFT", "TSLA", "GOOG", "FB", "EIX", "GS")) %>%
  ggplot(aes(x = sentiment_score, fill=symbol, color=symbol)) +
  geom_bar() 


load("/Users/poojasingh/Documents/HE107/E107project/pulkit/yahoo-finance.RData")
View(stocks)

utc.date <- as.POSIXct(data$createdat, tz="UTC")
head(utc.date)
est.date <- format(utc.date, tz="EST")
head(est.date)
