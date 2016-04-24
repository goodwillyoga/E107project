library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
tickers_symbols <- c("GILD","EIX","GS","AMZN", "RKUS","AAPL","GRPN","XIV","YHOO","VA","MSFT","TSLA","BSX","NVDA","ORCL","EW","CPGX","MRK","V","BXLT","FOXA","ERIC","AVP","TWX","CMCSA","XRX","WY","GNCA","WBA","MO","MA","FOLD","TLT","SNY","RTN","UTX","LOW","MAS","GPT","RICE","IBM","KHC","CDNS","ANTM","HD","INO","OCLR","LULU","SABR","DYN","AXLL","WEN","COH","GOOG","FB","TWTR","XOM","PSX","VLO","PGR","CINF","FAF","JBLU","DAL","HA","ACN","INFY","CTSH")

existingStocksDataLocation <- "/code/CSCIE-107/E107project/pulkit/yahoo-finance.RData"
existingTweetsDataLocation <- "/code/CSCIE-107/E107project/pulkit/twitter.RData"
null_value <- 0
load(existingTweetsDataLocation)
load(existingStocksDataLocation)

# remove all null's in stocks for columns open, daysHigh, daysLow with 0
stocks %>% mutate( open=ifelse(open!='null', open,null_value), 
                  daysHigh = ifelse(daysHigh!='null', daysHigh,null_value), 
                  daysLow = ifelse(daysLow != 'null', daysLow, null_value )) 

filter(stocks, symbol=='CGPX' & !is.numeric(daysHigh))

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
# The date we recieve from yahoo finance is of the form 2:54PM, let us convert it into 14:54 and take away the PM part
tmp1<- stocks %>% mutate(date_time = paste(lastTradeDate,convert_24Time(lastTradeTime)))
# use lubridate and convert to date_time
tmp1 <- tmp1 %>% mutate(date_timelb = mdy_hm(date_time), daysHigh = as.numeric(daysHigh), daysLow = as.numeric(daysLow), open = as.numeric(open), prvClose= as.numeric(prvClose))


tmp1 <- tmp1 %>% filter(!is.na(daysHigh))
tmp1 <- distinct(tmp1)
ggplot(tmp1, aes(date_timelb,price, color=symbol))+ geom_line()+scale_y_log10()


####################### Below is specific to tweets #############################
# Convert the date to New_york timezone as all the stock prices are also available in that timezone
tweets_tmp <- mutate(tweets, date_timelb = as.POSIXct(as.numeric(time_stamp)/1000, origin="1970-01-01",tz = "America/New_York"))

# Let us do EDA on the tweets
tweets_tmp %>% group_by(user_id) %>% summarize(count=n()) %>% filter(count >5) %>% ggplot(data=., aes(log(count)))+geom_histogram()

# Let us see who are the top 40 authors
tweets_tmp %>% group_by(user_id) %>% summarize(count=n()) %>% filter(count >5) %>% inner_join(users) %>% arrange(desc(count)) %>% print(n=40)


# Load a list of positive and -ve words 
pos <- scan('/code/CSCIE-107/E107project/pulkit/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('/code/CSCIE-107/E107project/pulkit/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

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
scores <- score.sentiment(tweets$text, pos.words, neg.words, .progress='text')
stat <- scores
# add the timestamp of the score
stat$created <- tweets_tmp$date_timelb
# add the id of the str
stat$id_Str <- tweets_tmp$id_str
# classify them to positive/nagative 
stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, created) %>% summarise( number=n())
by.tweet <- 

ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) 

hist(stat$score)

write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)

View(scores)
