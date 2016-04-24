library(dplyr)
library(readr)

column_names<-c("symbol","price","lastTradeDate","lastTradeTime","daysHigh","daysLow","volume","yearHigh","yearLow","open","prvClose")

# File Location where the dump is to be saved, If a file already exists we load the existing data and merge the new data to it and write back to this location
existingStocksDataLocation <- "/code/CSCIE-107/E107project/pulkit/yahoo-finance.RData"
# File location for the zipped files, when the files are processed they are moved from this directory to a new location using  getDestinationFileName function
files <- list.files("/code/tot/Info/twitter-data/unprocessesed",pattern = "^stocks.csv",full.names = TRUE)

getDestinationFileName <- function(fileName, replaceWord="unprocessesed", withWord="processesed"){
  gsub(replaceWord, withWord, fileName)
}

# Function to create the directory path if the path doesnt exist
createDirectoryTree <- function(toFileName) {
  todir <- dirname(toFileName)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
}
if(file.exists(existingStocksDataLocation)){
  # Load the existing data, this will result in creation of dataframes for tweets, hashtags, symbols, users
  load(existingStocksDataLocation)
}else{
stocks<- data.frame(symbol=character(),price=numeric(),lastTradeDate=character(),lastTradeTime=character(),daysHigh=character(),daysLow=character(),
                    volume=numeric(),yearHigh=numeric(),yearLow=numeric(),open=character(),prvClose=numeric(),stringsAsFactors=FALSE) 
}
#set open to 0 for missing.
#set dayhigh daylow to 0 for missing

if(length(files)){
  for( j in 1:length(files)){
    tmp <- read_csv(gzfile(files[j]),col_names = FALSE, col_types = list(X1=col_character(),X2=col_number(),X3=col_character(),X4=col_character(),X5=col_character(),X6=col_character(),
                                                                         X7=col_number(),X8=col_number(),X9=col_number(),X10=col_character(),X11=col_number()))
    cat("File Processed ", files[j], "rows read", nrow(tmp),"\n")
    
    colnames(tmp) <- column_names
    stocks <- bind_rows(stocks,tmp)
    newName <- getDestinationFileName(files[j])
    createDirectoryTree(newName)
    file.rename(files[j], newName)
    cat("Moving file", files[j], " to ",newName,"\n")
  }
}

save(stocks, file = existingStocksDataLocation)


