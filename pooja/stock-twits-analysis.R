## ---- messages=FALSE, warning=FALSE, echo=FALSE--------------------------
suppressMessages(library(dplyr))
suppressMessages(library(readr))
#opts_chunk$set(cache = TRUE, message = FALSE)

suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(gtable))
suppressMessages(library(broom))
suppressMessages(library(ggrepel))
# Disable logging of all warning messages
options(warn=-1)
theme_set(theme_bw(base_size = 14))
suppressMessages(library(broom))
suppressMessages(library(knitr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(portfolio))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))



## ---- warning=FALSE------------------------------------------------------
data <- read_csv(file = "https://raw.githubusercontent.com/goodwillyoga/E107project/master/pooja/stock_twits_sentiment_score_na.csv")

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
# Following is the list of all the stocks which we have downloaded the tweets and the stocks from Yahoo finance
tickers_symbols <- c("GILD","EIX","GS","AMZN", "RKUS","AAPL","GRPN","XIV","YHOO","VA","MSFT","TSLA","BSX","NVDA","ORCL","EW","CPGX","MRK","V","BXLT","FOXA","ERIC","AVP","TWX","CMCSA","XRX","WY","GNCA","WBA","MO","MA","FOLD","TLT","SNY","RTN","UTX","LOW","MAS","GPT","RICE","IBM","KHC","CDNS","ANTM","HD","INO","OCLR","LULU","SABR","DYN","AXLL","WEN","COH","GOOG","FB","TWTR")
# Sectors associate with each stock
sectors <- c("Healthcare","Utilities","Financial","Services","Technology","Consumer Goods","Technology","Financial","Technology","Services","Technology","Consumer Goods","Healthcare","Technology","Technology","Healthcare","Basic Materials","Healthcare","Financial","Healthcare","Services","Telecommunications","Consumer Goods","Services","Services","Technology","Industrial Goods","Healthcare","Services","Consumer Goods","Financial","Healthcare",
             "Financial","Healthcare","Industrial Goods","Industrial Goods","Services","Industrial Goods","Financial","Basic Materials","Technology","Consumer Goods","Technology","Healthcare","Services","Healthcare","Technology","Consumer Goods","Technology","Utilities","Basic Materials","Services","Consumer Goods","Technology","Technology","Technology")

selectively_analyzed_symbols <- c("GS","IBM","EIX")
ticker_sector <- data.frame(symbol = tickers_symbols, sector = sectors)

## ----load stocks, messages=FALSE, warning=FALSE, echo=FALSE--------------
#Load YAHOO finance data
load(url("https://github.com/goodwillyoga/E107project/raw/master/pulkit/yahoo-finance.RData"))

colnames(data) <- c('id','message','createdat','symbol','sentiment_score')

#Convert to UTC to EST time zone 
data <- data %>% mutate(utc.time = as.POSIXct(data$createdat, tz="UTC")) %>%
  mutate(est.time = format(utc.time, tz="America/New_York")) %>%
  mutate(est.date = substr(as.character(format(strftime(est.time, '%m/%d/%Y'))), 2,10)) %>%
  mutate(est.hour = paste(est.date, lubridate::hour(est.time)))

#Get hour from time function
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

normalize<-function(m){
  (m - mean(m))/sd(m)
}

#Average sentiment score by day
avg_sentiment_score_by_day <- data %>% 
  group_by(symbol, day=est.date) %>%
  summarise(avg_score = mean(sentiment_score), tweet_counts = n()) %>%
  ungroup() %>%
  group_by(symbol) %>%
  mutate(scale_avg_score = normalize(avg_score)) %>% 
  mutate(scale_tweet_counts = normalize(tweet_counts)) %>% 
  select(symbol, day, avg_score, tweet_counts, scale_avg_score, scale_tweet_counts) %>%
  unique() 

avg_stocks_price_by_day <- stocks %>% 
  group_by(symbol, day=lastTradeDate) %>%
  group_by(day,symbol) %>% 
  filter(volume == max(volume)) %>% # Select last record for the day
  mutate(price_change = as.numeric(open) - price) %>% #Price change for a day
  ungroup() %>%
  group_by(symbol) %>%
  mutate(scale_price = normalize(price)) %>% 
  mutate(scale_price_change = normalize(price_change)) %>% 
  mutate(scale_volume = normalize(volume)) %>% 
  ungroup() %>%
  select(symbol, day, price, scale_price, scale_price_change, scale_volume) %>%
  unique()

#Join score and price
dat.byday <- inner_join(avg_stocks_price_by_day, avg_sentiment_score_by_day)

#EDA for positive words and negative words 
#Count all positive words that were matched in stocktwits messages
posWords <- read_csv(file = "https://raw.githubusercontent.com/goodwillyoga/E107project/master/pooja/posWords_na.csv")
colnames(posWords)[1] <- c('words')
posWords <- posWords %>% group_by(words) %>% summarize(counts=n())
len <- length(posWords$words)
posWords <- cbind(posWords, rep('positive',len))
colnames(posWords)[3] <- c('sentiment')

#Count all negative words that were matched in message stocktwits messages
negWords <- read_csv(file = "https://raw.githubusercontent.com/goodwillyoga/E107project/master/pooja/negWords_na.csv")
colnames(negWords)[1] <- c('words')
negWords <- negWords %>% group_by(words) %>% summarize(counts=n())
len <- length(negWords$words)
negWords <- cbind(negWords, rep('negative',len))
colnames(negWords)[3] <- c('sentiment')
 
#Combine al the positive and nagative words and select top 20 frquently used positive words and negative words
words <- rbind(posWords, negWords)
freqwords <- words %>% filter(sentiment=='positive') %>% top_n(50,counts)
freqwords <- rbind(freqwords, words %>% filter(sentiment=='negative') %>% top_n(50,counts))

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9--------------
dat.plot <- 
  dat.byday %>% select(symbol, day, price, tweet_counts) %>%
  filter(day == "4/19/2016") %>%
  mutate(price=round(price)) %>%
  select(symbol, round(price), tweet_counts) 

map.market(id=dat.plot$symbol, area=dat.plot$price, group=dat.plot$symbol, color=dat.plot$tweet_counts, main="Stock Price and Tweet counts", lab=c(FALSE,TRUE))

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9--------------
map.market(id=freqwords$words, area=freqwords$counts, group=freqwords$words, color=freqwords$counts, main="Top 50 Positive and Negative Sentiment Words", lab=c(FALSE,TRUE))

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.height=3-------------
hist(data$sentiment_score, main='Sentiment score', xlab='Sentiment score')

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
library(lubridate)
# Let us model closing price vs count for each stock
fits <- dat.byday %>%
  group_by(symbol) %>%
  do(mod = lm(scale_price ~ scale_tweet_counts, data = .))

# Filter out rows where term is count
results_price_count <- tidy(fits,mod, conf.int = TRUE) %>% 
  filter(term=='scale_tweet_counts') %>% 
  inner_join(ticker_sector)
colnames(results_price_count)[6]<- 'pval'

# Plot for all stocks
p1 <- results_price_count %>% filter(pval <.15) %>% ggplot( aes(symbol, pval, color=sector)) + 
  geom_point() + geom_hline(yintercept = .05, color = 'red') +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(size = rel(0.75))) + 
  scale_y_continuous(limits = c(0, .15))+
  ggtitle("ClosingPrice~Count")+
  geom_text_repel(aes(symbol, pval, label=symbol), data = filter(results_price_count,  pval <.05 )) +
  theme(legend.position="bottom")

# Model volume ~ count for daily data
fits <- dat.byday %>%
  group_by(symbol) %>%
  do(mod = lm(scale_volume ~ scale_tweet_counts, data = .))

# Filter out rows where term is count
results_volume_count <- tidy(fits,mod, conf.int = TRUE) %>% filter(term=='scale_tweet_counts') %>% inner_join(ticker_sector)
colnames(results_volume_count)[6]<- 'pval'

# Plot for all stocks and highlight the stocks with pvals <.05
p2<- results_volume_count %>% filter(pval <.15) %>% ggplot( aes(symbol, pval, color=sector)) + 
  geom_point() + geom_hline(yintercept = .05, color = 'red') +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks=element_blank(),
        axis.text.y=element_blank(), 
        axis.title.y=element_blank(),
        plot.title=element_text(size = rel(0.75))) +
  scale_y_continuous(limits = c(0, .15))+
  ggtitle("Volume~Count") + xlab("Symbols") + ylab("Pvalues")+
  geom_text_repel(aes(symbol, pval, label=symbol), data = filter(results_volume_count,  pval <.05 )) +
  theme(legend.position="bottom")

# Model Price Change vs count for all stocks
fits <- dat.byday %>%
  group_by(symbol) %>%
  do(mod = lm(scale_price_change ~ scale_tweet_counts, data = .))

# Filter out rows where term is count
results_prcChange_count <- tidy(fits,mod, conf.int = TRUE) %>% 
  filter(term=='scale_tweet_counts') %>% inner_join(ticker_sector)

#Rename the column
colnames(results_prcChange_count)[6]<- 'pval'

# Plot for all stocks
p3<- results_prcChange_count %>% filter(pval <.15) %>% ggplot( aes(symbol, pval, color=sector)) + 
  geom_point() + geom_hline(yintercept = .05, color = 'red') +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks=element_blank(),
        axis.title.y=element_blank(),  
        axis.text.y=element_blank(),
        plot.title=element_text(size = rel(0.75))) +
  scale_y_continuous(limits = c(0, .15))+ 
  ggtitle("PriceChange~Count") + xlab("Symbols") + ylab("Pvalues") + 
  geom_text_repel(aes(symbol, pval, label=symbol), data = filter(results_prcChange_count,  pval <.05 )) +
  theme(legend.position="bottom")

# Extract the legend from p1
legend = gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box") 

grid.arrange(arrangeGrob(p1+ 
                           theme(legend.position="none"), p2+ 
                           theme(legend.position="none"), p3+ 
                           theme(legend.position="none"), nrow=1),top = "P-Value for models Using Daily Normalized Data")

## ----echo=FALSE, message=FALSE, fig.width=9, fig.height=.5---------------
grid.newpage()
grid.draw(legend) 

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
# Let us plot specific for the 3 stocks 
p1 <- results_price_count %>% 
  filter(symbol %in% selectively_analyzed_symbols) %>% 
  ggplot( aes(symbol,pval, color=symbol))+geom_point()+ scale_y_continuous(limits = c(0, 1))+
  geom_hline(yintercept = .05, color='red') + 
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Symbols") + ylab("Pvalues") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("ClosingPrc~Count")+ theme(legend.position="bottom")

p2<- results_volume_count %>% 
  filter(symbol %in% selectively_analyzed_symbols) %>% 
  ggplot( aes(symbol, pval, color=symbol)) + geom_point() + scale_y_continuous(limits = c(0, 1))+ 
  geom_hline(yintercept = .05, color = 'red') + 
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Symbols") + ylab("Pvalues") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Volume~Count")+ theme(legend.position="bottom")

p3<- results_prcChange_count %>% 
  filter(symbol %in% selectively_analyzed_symbols)  %>% 
  ggplot( aes(symbol,pval, color=symbol))+geom_point()+ scale_y_continuous(limits = c(0, 1))+
  geom_hline(yintercept = .05, color='red') + 
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Symbols") + ylab("Pvalues") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("DailyPrcChn~Count")+ theme(legend.position="bottom")

grid.arrange(arrangeGrob(p1+ 
                           theme(legend.position="none"), p2+ 
                           theme(legend.position="none"), p3+ 
                           theme(legend.position="none"), nrow=1), top = "P-Value for models Using Daily Normalized Data")

## ----echo=FALSE, message=FALSE, fig.width=9, fig.height=.5---------------
grid.newpage()
grid.draw(legend) 

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
# Let us plot the confidence intervals
p1 <-results_price_count %>% 
  filter(symbol %in% selectively_analyzed_symbols) %>% 
  select(symbol,estimate, pval,`conf.low`, `conf.high`) %>% 
  ggplot( aes(x = symbol, y = estimate, color = symbol)) +
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  geom_point(size = 2) + ggtitle("ClosingPrc~Count")+ xlab("Symbols") + ylab("Conf. Interval") + 
  geom_errorbar(aes(ymax = `conf.high`, ymin = `conf.low`))

p2 <- results_volume_count %>% 
  filter(symbol %in% selectively_analyzed_symbols) %>% 
  select(symbol,estimate, pval,`conf.low`, `conf.high`) %>% 
  ggplot( aes(x = symbol, y = estimate, color = symbol)) +
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  geom_point(size = 2) + ggtitle("Volume~Count")+ xlab("Symbols") + ylab("Conf. Interval") + 
  geom_errorbar(aes(ymax = `conf.high`, ymin = `conf.low`))

p3 <-results_prcChange_count %>% 
  filter(symbol %in% selectively_analyzed_symbols) %>% 
  select(symbol,estimate, pval,`conf.low`, `conf.high`) %>% 
  ggplot( aes(x = symbol, y = estimate, color = symbol)) +
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  geom_point(size = 2) + ggtitle("DailyPrcChn~Count")+ xlab("Symbols") + ylab("Conf. Interval") + 
  geom_errorbar(aes(ymax = `conf.high`, ymin = `conf.low`))

grid.arrange(arrangeGrob(p1+ 
                           theme(legend.position="none"), p2+ 
                           theme(legend.position="none"), p3+ 
                           theme(legend.position="none"), nrow=1), top = "Confidence Interval Plots")

## ----echo=FALSE, message=FALSE, fig.width=9, fig.height=.5---------------
grid.newpage()
grid.draw(legend) 

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
# Do regression plots for the 3 stocks based on volume~count
dat.byday %>% filter(symbol %in% selectively_analyzed_symbols) %>% ggplot( aes(scale_tweet_counts,scale_price)) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Tweet Counts") + ylab("Price") +
  geom_point()+geom_smooth(color='red') +facet_wrap(~symbol)

# Do regression plots for the 3 stocks based on volume~count
dat.byday %>% filter(symbol %in% selectively_analyzed_symbols) %>% ggplot( aes(scale_tweet_counts,scale_volume)) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Tweet Counts") + ylab("Volume") +
  geom_point()+geom_smooth(color='red') +facet_wrap(~symbol)

# Do regression plots for the 3 stocks based on Daily Price change~count
dat.byday %>% filter(symbol %in% selectively_analyzed_symbols) %>% ggplot( aes(scale_tweet_counts,scale_price_change))+
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Tweet Conts") + ylab("Price Change") +
  geom_point()+geom_smooth(color='red') +facet_wrap(~symbol)

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
dat.byday %>% filter(symbol %in% selectively_analyzed_symbols) %>% mutate(week = lubridate::isoweek(mdy(day))) %>% 
  ggplot()+geom_line(aes(mdy(day),scale_tweet_counts, color="Sentiment Score")) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Day") + ylab("Price/Tweet Counts") +
  geom_line(aes(mdy(day),scale_price, color="Price")) + facet_wrap(~symbol,scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme(legend.position="bottom")

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
library(lubridate)
# Let us model closing price vs SentimentScore for each stock
fits <- dat.byday %>%
  group_by(symbol) %>%
  do(mod = lm(scale_price ~ scale_avg_score, data = .))

results_price <- tidy(fits,mod, conf.int = TRUE) %>% filter(term=='scale_avg_score') %>% inner_join(ticker_sector)
colnames(results_price)[6]<- 'pval'

# Plot for all stocks
p1 <- results_price %>% filter(pval <.5) %>% ggplot( aes(symbol, pval, color=sector)) + 
  geom_point() + geom_hline(yintercept = .05, color = 'red') + xlab("Symbols") + ylab("P-values") + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks=element_blank(),
        axis.title.y=element_blank(),        
        plot.title=element_text(size = rel(0.75))) + 
  ggtitle("ClosingPrc~Score") + scale_y_continuous(limits = c(0, .5))+
  geom_text_repel(aes(symbol, pval, label=symbol), data = filter(results_price,  pval <.05 )) +
  theme(legend.position="bottom")

# Model volume ~ sentiment score
fits <- dat.byday %>%
  group_by(symbol) %>%
  do(mod = lm(scale_volume ~ scale_avg_score, data = .))

# Filter out rows where term is avgScore
results_volume <- tidy(fits,mod, conf.int = TRUE) %>% filter(term=='scale_avg_score') %>% inner_join(ticker_sector)
#Rename the column
colnames(results_volume)[6]<- 'pval'

# Plot for all stocks
p2 <- results_volume%>% filter(pval <.5) %>% ggplot( aes(symbol, pval, color=sector)) + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),        
        plot.title=element_text(size = rel(0.75))) +
  geom_point() + geom_hline(yintercept = .05, color = 'red') + xlab("Symbols") + ylab("P-values") +
  ggtitle("Volume~Score") + scale_y_continuous(limits = c(0, .5))+
  geom_text_repel(aes(symbol, pval, label=symbol), data = filter(results_volume,  pval <.05 )) +
  theme(legend.position="bottom")

# Model Price Change vs Sentimenet Score for all stocks
fits <- dat.byday %>%
  group_by(symbol) %>%
  do(mod = lm(scale_price_change ~ scale_avg_score, data = .))

results_prcChange <- tidy(fits,mod, conf.int = TRUE) %>% filter(term=='scale_avg_score') %>% inner_join(ticker_sector)
colnames(results_prcChange)[6]<- 'pval'

# Plot for all stocks
p3<- results_prcChange%>% filter(pval <.5) %>% ggplot(aes(symbol, pval, color=sector)) + 
  geom_point() + geom_hline(yintercept = .05, color = 'red') + xlab("Symbols") + ylab("P-values") +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),        
        plot.title=element_text(size = rel(0.75))) +
  ggtitle("DailyPrcChn~Score") + scale_y_continuous(limits = c(0, .5))+
  geom_text_repel(aes(symbol, pval, label=symbol), data = filter(results_prcChange,  pval <.05 )) +
  theme(legend.position="bottom")

# Extract the legend from p1
legend = gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box") 

grid.arrange(arrangeGrob(p1+ 
                           theme(legend.position="none"), p2+ 
                           theme(legend.position="none"), p3+ 
                           theme(legend.position="none"), nrow=1),top = "P-Value for models Using Daily Normalized Data")

## ----echo=FALSE, message=FALSE, fig.width=9, fig.height=.5---------------
grid.newpage()
grid.draw(legend) 

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
# Let us plot specific for the 3 stocks 
p1 <- results_price %>% filter(symbol %in% selectively_analyzed_symbols)  %>% 
  ggplot( aes(symbol,pval, color=symbol))+geom_point()+ xlab("Symbols") + ylab("P-values") +
  geom_hline(yintercept = .05, color='red') + 
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("ClosingPrc~Score")+ theme(legend.position="bottom")


p2<- results_volume %>% filter(symbol %in% selectively_analyzed_symbols) %>% 
  ggplot( aes(symbol, pval, color=symbol)) + geom_point() + xlab("Symbols") + ylab("P-values") +
  geom_hline(yintercept = .05, color = 'red') + 
  theme(plot.title=element_text(size = rel(0.75))) + 
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Volume~Score")+ theme(legend.position="bottom")


p3<- results_prcChange %>% filter(symbol %in% selectively_analyzed_symbols)  %>% 
  ggplot( aes(symbol,pval, color=symbol))+geom_point()+ xlab("Symbols") + ylab("P-values") +
  geom_hline(yintercept = .05, color='red') + 
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("DailyPrcChange~Score")+ theme(legend.position="bottom")

grid.arrange(arrangeGrob(p1+ 
                           theme(legend.position="none"), p2+ 
                           theme(legend.position="none"), p3+ 
                           theme(legend.position="none"), nrow=1), top = "P-Value for Normalized Daily Data")

# Let us plot the confidence intervals
p1 <-results_price %>% 
  filter(symbol %in% selectively_analyzed_symbols) %>% 
  select(symbol,estimate, pval,`conf.low`, `conf.high`) %>% 
  ggplot( aes(x = symbol, y = estimate, color = symbol)) +
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point(size = 2) + ggtitle("ClosingPrc~Score")+ xlab("Symbols") + ylab("Conf. Interval") + 
  geom_errorbar(aes(ymax = `conf.high`, ymin = `conf.low`))

p2 <- results_volume %>% 
  filter(symbol %in% selectively_analyzed_symbols) %>% 
  select(symbol,estimate, pval,`conf.low`, `conf.high`) %>% 
  ggplot( aes(x = symbol, y = estimate, color = symbol)) +
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point(size = 2) + ggtitle("Volume~Score")+ xlab("Symbols") + ylab("Conf. Interval") + 
  geom_errorbar(aes(ymax = `conf.high`, ymin = `conf.low`))

p3 <-results_prcChange %>% 
  filter(symbol %in% selectively_analyzed_symbols) %>% 
  select(symbol,estimate, pval,`conf.low`, `conf.high`) %>% 
  ggplot( aes(x = symbol, y = estimate, color = symbol)) +
  theme(plot.title=element_text(size = rel(0.75))) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point(size = 2) + ggtitle("DailyPrcChn~Score")+ xlab("Symbols") + ylab("Conf. Interval") + 
  geom_errorbar(aes(ymax = `conf.high`, ymin = `conf.low`))

grid.arrange(arrangeGrob(p1+ 
                           theme(legend.position="none"), p2+ 
                           theme(legend.position="none"), p3+ 
                           theme(legend.position="none"), nrow=1), top = "Confidence Interval Plots")

## ----echo=FALSE, message=FALSE, fig.width=9, fig.height=.5---------------
grid.newpage()
grid.draw(legend) 

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
# Do regression plots for the 3 stocks based on price~score
dat.byday %>% filter(symbol %in% selectively_analyzed_symbols) %>% ggplot( aes(scale_avg_score,scale_price))+
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Sentiment Score") + ylab("Price") +
  geom_point()+geom_smooth(color='red') +facet_wrap(~symbol)

# Do regression plots for the 3 stocks based on volume~score
dat.byday %>% filter(symbol %in% selectively_analyzed_symbols) %>% ggplot( aes(scale_avg_score,scale_volume))+
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Sentiment Score") + ylab("Volume") +
  geom_point()+geom_smooth(color='red') +facet_wrap(~symbol)

# Do regression plots for the 3 stocks based on Daily Price change~score
dat.byday %>% filter(symbol %in% selectively_analyzed_symbols) %>% ggplot( aes(scale_avg_score,scale_price_change))+
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Sentiment Score") + ylab("Price Change") +
  geom_point()+geom_smooth(color='red') +facet_wrap(~symbol)

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.width=9, fig.height=3----
dat.byday %>% filter(symbol %in% selectively_analyzed_symbols) %>% mutate(week = lubridate::isoweek(mdy(day))) %>% 
  ggplot()+geom_line(aes(mdy(day),scale_avg_score, color="Sentiment Score")) +
  theme(axis.title.x = element_text(size = rel(0.75))) +
  theme(axis.title.y = element_text(size = rel(0.75))) +
  xlab("Day") + ylab("Price/Sentiment Score") +
  geom_line(aes(mdy(day),scale_price, color="Price")) + facet_wrap(~symbol,scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme(legend.position="bottom")

