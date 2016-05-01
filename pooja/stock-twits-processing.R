library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

#Passing variable by reference 
e <- new.env(parent=globalenv()) 
#Turn off warnigs
options(e$warn=-1)

e$dat <- c()

paste("Start aggregating data...")
Sys.time()
aggregate_data  <-  suppressWarnings(lapply(1:64, function(i) {
    path = ""
    tryCatch({
      
      e$path <- paste("https://raw.githubusercontent.com/goodwillyoga/E107project/master/pooja/data/stock-twits-multiple-", i, ".csv", sep="")
      #path
      sprintf("path: %s", e$path)
      
      #Read data
      temp <- read_csv(e$path)
      paste("before deduplicate iteration ", i, " length(temp$id)=", length(temp$id))
      
      #Rename columns
      names(temp) <-c("id", "message", "created-at", "symbol")
      
      #Clean up the duplicate messages
      temp <- subset(temp,!duplicated(temp$id))
      paste("after deduplicate iteration ", i, " length(temp$id)=", length(temp$id))
      
      
      #Aggregate data
      e$dat <- rbind(e$dat, temp)
      
      paste("iteration ", i, " length(e$dat$id)=", length(e$dat$id))
      
    }, error = function(e1) {
      cat("error in path ", e$path, message(e1), fill=TRUE)
      NULL
    })
}))
paste("End aggregating data...!")
Sys.time()

str(e$dat)

#AFINN is a list of English words rated for valence with an integer between minus five (negative) and plus five (positive)
afinn_list <- read.delim(file = "/Users/poojasingh/Documents/HE107/E107project/pooja/AFINN-111.txt", header = FALSE, stringsAsFactors = FALSE)  
names(afinn_list) <- c("word", "score")  
afinn_list$word <- tolower(afinn_list$word)

#Storing the positive and negative terms in enviornment
e$negTerms <- c(afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4 | afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1])
e$posTerms <- c(afinn_list$word[afinn_list$score==1 | afinn_list$score==2 | afinn_list$score==3 |afinn_list$score==4 | afinn_list$score==5])  
length(e$negTerms)
length(e$posTerms)

pos <- scan('/Users/poojasingh/Documents/HE107/E107project/pulkit/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('/Users/poojasingh/Documents/HE107/E107project/pulkit/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
pos <- c(pos, 'upgrade', 'up', 'rise', 'rising', 'above', 'success')
neg <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail', 'down', 'fall', 'falling', 'fail', 'failing', 'failure', 'down', 'doom', 'doomed', 'below', 'downgrade')
length(neg)
length(pos)

#Combine the AFINN and opinion-lexicon lists
e$posTerms  <- unique(c(e$posTerms, pos))
e$negTerms <- unique(c(e$negTerms, neg))
length(e$negTerms)
length(e$posTerms)

#Temp variables
e$dat2 <- e$dat
e$dat3  <- c()
e$posWords  <- c()
e$negWords  <- c()

paste("Start calculating sentiment score...")
Sys.time()
dat.length <- length(e$dat$message)
sentiment_scores <- lapply(1:dat.length, function(i){
 
          #remove unnecessary characters and split up by word 
          sentence <- e$dat$message[i] 
          sentence
          sentence <- gsub('[[:punct:]]', '', sentence)
          sentence <- gsub('[[:cntrl:]]', '', sentence)
          sentence <- gsub('\\d+', '', sentence)
          sentence <- tolower(sentence)
          wordList <- str_split(sentence, '\\s+')
          words <- unlist(wordList)
          
          #Bag of positive words for inforgraphics later
          posTerms <- as.data.frame(e$posTerms[match(words, e$posTerms)]) %>% filter(!is.na(.))
          posTerms <- as.character(posTerms[1,])
          e$posWords <- c(e$posWords, posTerms)
          
          #Bag of negative words for inforgraphics later
          negTerms <- as.data.frame(e$negTerms[match(words, e$negTerms)]) %>% filter(!is.na(.))
          negTerms <- as.character(negTerms[1,])
          e$negWords <- c(e$negWords, negTerms)
          
          #build vector with matches between sentence and each category
          vPosMatches <- sum(!is.na(match(words, e$posTerms)) > 0)
          vNegMatches <- sum(!is.na(match(words, e$negTerms)) > 0)
          vPosMatches
          vNegMatches
          score <- vPosMatches - vNegMatches
          score
          e$dat3 <- rbind(e$dat3,cbind(e$dat2[i,], "sentiment_score" = score))
          #paste("i ", i, "score " , score)
})
paste("End calculating sentiment score...")
Sys.time()

head(e$dat3)

write.csv(e$dat3, "/Users/poojasingh/stock_twits_sentiment_score_n1.csv")
write.csv(table(e$posWords), "/Users/poojasingh/posWords_n1.csv")
write.csv(table(e$negWords), "/Users/poojasingh/negWords_n1.csv")

hist(e$dat3$sentiment_score)

e$dat3 %>% filter(symbol %in% c("APPL", "YHOO", "MSFT", "TSLA", "GOOG", "FB", "EIG", "GS", "IBM")) %>%
  ggplot(aes(x = sentiment_score, fill=symbol, color=symbol)) +
  geom_bar() 

e$dat3 %>%
  ggplot(aes(symbol,sentiment_score, fill=symbol, color=symbol)) + geom_point()
str(e$dat3)
