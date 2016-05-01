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

# Load the twitter datset into memory
load(url("https://github.com/goodwillyoga/E107project/raw/master/pulkit/twitter.RData"))

# Change the column name to symbol
colnames(symbols)[2] <- "symbol"

#Function to normalize the values
normalize<-function(m){
  (m - mean(m))/sd(m)
}

#Function to find %age change between consecutive values
pcchange=function(x,lag=1) {
  c(diff(x,lag))/x
}

# This script will make 4 datasets, 
# 1 for tweets with the dates converted to EST named tweets_est
# 1 for the sentiment scores converted for each tweet named scores
# 1 for hourly sentiment score per stock named hourly_tweet_score
# 1 for daily sentiment score per stock named daily_tweet_score 

# create a dataset for hourly number of tweets for each stock
# Convert the date to New_york timezone as all the stock prices are also available in that timezone
# Tweets are stored in tweets dataset, and the symbols in the tweets are stored in the symbols dataset.
# Join the tweets_est dataset with symbols dataset and filter the data set to only contain required stock symbols
# Filter out tweets only for the selective days
tweets_est <- mutate(tweets, date_timelb = as.POSIXct(as.numeric(time_stamp)/1000, origin="1970-01-01",tz = "America/New_York")) %>% 
  mutate(day_hr = make_datetime(year = year(date_timelb), month = month(date_timelb), day = day(date_timelb), hour=hour(date_timelb)),dt = date(date_timelb)) %>% 
  inner_join(symbols) %>% 
  inner_join(ticker_sector) 

# Load the positive and negative words for sentiment generation
pos <- scan('https://github.com/goodwillyoga/E107project/raw/master/pulkit/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('https://github.com/goodwillyoga/E107project/raw/master/pulkit/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

# Function to calculate the sentiment score for tweets
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  scores <- plyr::laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence) 
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
# calculate the sentiment score
scores <- score.sentiment(tweets_est$text, pos.words, neg.words, .progress='text')
# add the timestamp of the score
scores$created <- tweets_est$date_timelb
# add the id of the str
scores$id_str <- tweets_est$id_str
# classify them to positive/nagative 
scores <- mutate(scores, tweet=ifelse(scores$score > 0, 'positive', ifelse(scores$score < 0, 'negative', 'neutral')))

# Let us add the symbol column to the scores to make it easy to join in the later evaluations
scores<- inner_join(scores,symbols) %>% filter(symbol %in% tickers_symbols)
# Now that we have generated the tweet scores, making dataset daily_tweet_score
daily_tweet_score <- scores %>% 
  mutate(day = as_date(created)) %>% 
  group_by(symbol,day) %>% summarise(avgScore = mean(score)) %>% group_by(symbol) %>% 
  mutate_each(funs(normalize), avgScore) %>% ungroup()

# Now making a dataset for hourly tweet score 
hourly_tweet_score <- scores %>%
  mutate(day_ms = ymd_hms(created)) %>% 
  mutate(day_hr = make_datetime(year = year(day_ms), month = month(day_ms), day = day(day_ms), hour=hour(day_ms)), day = as.Date(day_ms)) %>% 
  group_by(symbol,day, day_hr) %>% 
  summarise(avgScore = mean(score)) %>% group_by(symbol,day) %>% mutate(n=n()) %>%  filter(n>1) %>%
  mutate_each(funs(normalize), avgScore) %>% ungroup()

# Let us store the 4 datasets into a .rdata file 
save(tweets_est, scores, daily_tweet_score,hourly_tweet_score,  file = "/code/CSCIE-107/E107project/pulkit/processed-tweets.RData")

