library(dplyr)
library(readr)
library(stringr)


#Passing variable by reference 
e <- new.env(parent=globalenv()) 
#Turn off warnigs
options(e$warn=-1)

e$dat <- c()

aggregate_data  <-  suppressWarnings(lapply(1:47, function(i) {
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

str(e$dat)

#AFINN is a list of English words rated for valence with an integer between minus five (negative) and plus five (positive)
afinn_list <- read.delim(file = "AFINN-111.txt", header = FALSE, stringsAsFactors = FALSE)  
names(afinn_list) <- c("word", "score")  
afinn_list$word <- tolower(afinn_list$word)

#Storing the positive and negative terms in enviornment
e$negTerms <- c(afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4 | afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1])
e$posTerms <- c(afinn_list$word[afinn_list$score==1 | afinn_list$score==2 | afinn_list$score==3 |afinn_list$score==4 | afinn_list$score==5])  

#Temp variables
e$dat2 <- e$dat
e$dat3  <- c()

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
words

#build vector with matches between sentence and each category
vPosMatches <- match(words, e$posTerms)
vNegMatches <- match(words, e$negTerms)
vPosMatches
vNegMatches

#sum up number of words in each category
posMatches <- sum(!is.na(vPosMatches))
negMatches <- sum(!is.na(vNegMatches))
score <- posMatches - negMatches
e$dat3 <- rbind(e$dat3,cbind(e$dat2[i,], "sentiment_score" = score))
paste("i ", i, "score " , score)
})

head(e$dat3)

