library(dplyr)
library(readr)



#Passing variable by reference 
e <- new.env(parent=globalenv()) 
#Turn off warnigs
options(e$warn=-1)

e$dat <- c()

aggregate_data  <-  suppressWarnings(lapply(1:48, function(i) {
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

#ite


