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

#Percent change function
pcchange=function(x,lag=1) {
  c(diff(x,lag))/x
}

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

# scale function
scale<-function(m){
  (m - mean(m))/sd(m)
}

#Average sentiment score by day
avg_sentiment_score_by_hour <- data %>% 
            group_by(symbol, dayhour = est.hour) %>%
            summarise(avg_score = mean(sentiment_score), tweet_counts = n()) %>%
            mutate(pchange_tweet_counts = pcchange(tweet_counts)) %>%
            mutate(pchange_avg_score = pcchange(avg_score)) %>%
            mutate(pchange_avg_score = ifelse(is.na(pchange_avg_score) | is.infinite(pchange_avg_score) |is.nan(pchange_avg_score), 0, pchange_avg_score)) %>%
            ungroup() %>%
            group_by(symbol) %>%
            mutate(scale_pchange_avg_score = scale(pchange_avg_score)) %>% 
            mutate(scale_pchange_avg_score = ifelse(is.na(scale_pchange_avg_score) | is.infinite(scale_pchange_avg_score) |is.nan(scale_pchange_avg_score), 0, scale_pchange_avg_score)) %>%
            ungroup() %>%
            select(symbol, dayhour, avg_score, pchange_tweet_counts, pchange_avg_score, scale_pchange_avg_score) %>%
            unique() 
head(avg_sentiment_score_by_hour$pchange_avg_score)
load("/Users/poojasingh/Documents/HE107/E107project/pulkit/yahoo-finance.RData")
#View(stocks)

avg_stocks_price_by_hour <- stocks %>% 
                   mutate(dayhour = paste(lastTradeDate,get_24hour(lastTradeTime))) %>%
                   group_by(symbol, dayhour) %>%
                   summarise(price = mean(price)) %>%
                   mutate(pchange_price_change = pcchange(price)) %>%
                   mutate(pchange_price_change = ifelse(is.na(pchange_price_change) | is.infinite(pchange_price_change) |is.nan(pchange_price_change), 0, pchange_price_change)) %>%
                   ungroup() %>%
                   group_by(symbol) %>%
                   mutate(scale_pchange_price_change = scale(pchange_price_change)) %>% 
                   mutate(scale_pchange_price_change = ifelse(is.na(scale_pchange_price_change) | is.infinite(scale_pchange_price_change) |is.nan(scale_pchange_price_change), 0, scale_pchange_price_change)) %>%
                   ungroup() %>%
                   select(symbol, dayhour, price, pchange_price_change, scale_pchange_price_change) %>%
                   unique()

#Join score and price
dat <- inner_join(avg_stocks_price_by_hour, avg_sentiment_score_by_hour)

dat2 <- subset(dat, symbol=="AAPL")
dat2 <- data.frame(dens = c(dat2$scale_pchange_price_change, dat2$scale_pchange_avg_score)
                  , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat2$scale_pchange_price_change)))
dat2 <- cbind(dat2, symbol="AAPL")

dat3 <- subset(dat, symbol=="EIX")
dat3 <- data.frame(dens = c(dat3$scale_pchange_price_change, dat3$scale_pchange_avg_score)
                   , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat3$scale_pchange_price_change)))
dat3 <- cbind(dat3, symbol="EIX")

dat4 <- subset(dat, symbol=="GS")
dat4 <- data.frame(dens = c(dat4$scale_pchange_price_change, dat4$scale_pchange_avg_score)
                   , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat4$scale_pchange_price_change)))
dat4 <- cbind(dat4, symbol="GS")

dat5 <- subset(dat, symbol=="IBM")
dat5 <- data.frame(dens = c(dat5$scale_pchange_price_change, dat5$scale_pchange_avg_score)
                   , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat5$scale_pchange_price_change)))
dat5 <- cbind(dat5, symbol="IBM")

dat6 <- subset(dat, symbol=="YHOO")
dat6 <- data.frame(dens = c(dat6$scale_pchange_price_change, dat6$scale_pchange_avg_score)
                   , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat6$scale_pchange_price_change)))
dat6 <- cbind(dat6, symbol="YHOO")

dat7 <- subset(dat, symbol=="MSFT")
dat7 <- data.frame(dens = c(dat7$scale_pchange_price_change, dat7$scale_pchange_avg_score)
                   , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat7$scale_pchange_price_change)))
dat7 <- cbind(dat7, symbol="MSFT")

dat8 <- subset(dat, symbol=="TSLA")
dat8 <- data.frame(dens = c(dat8$scale_pchange_price_change, dat8$scale_pchange_avg_score)
                   , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat8$scale_pchange_price_change)))
dat8 <- cbind(dat8, symbol="TSLA")

dat9 <- subset(dat, symbol=="GOOG")
dat9 <- data.frame(dens = c(dat9$scale_pchange_price_change, dat9$scale_pchange_avg_score)
                   , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat9$scale_pchange_price_change)))
dat9 <- cbind(dat9, symbol="GOOG")

dat10 <- subset(dat, symbol=="FB")
dat10 <- data.frame(dens = c(dat10$scale_pchange_price_change, dat10$scale_pchange_avg_score)
                   , lines = rep(c("scaled percent price change", "scaled percent sentiment score"), each = length(dat10$scale_pchange_price_change)))
dat10 <- cbind(dat10, symbol="FB")


datz <- rbind(dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10)
#Plot.
datz %>% 
  ggplot(aes(x = dens, fill = lines)) + geom_density(alpha = 0.5) + 
  facet_wrap(~symbol)

dat %>% filter(symbol %in% c("AAPL", "YHOO", "MSFT", "TSLA", "GOOG", "FB", "EIX", "GS", "IBM")) %>%
  ggplot(aes(x=pchange_avg_score)) + 
  geom_histogram(aes(y=..density..), bins=20, colour="black", fill="white") + 
  geom_density(alpha=0.2, fill="#FF6666") +
  facet_wrap(~symbol)

dat %>% filter(symbol %in% c("AAPL", "YHOO", "MSFT", "TSLA", "GOOG", "FB", "EIX", "GS", "IBM")) %>%
  ggplot(aes(x=pchange_price_change)) + 
  geom_histogram(aes(y=..density..), bins=20, colour="black", fill="white") + 
  geom_density(alpha=0.2, fill="#FF6666") +
  facet_wrap(~symbol)

