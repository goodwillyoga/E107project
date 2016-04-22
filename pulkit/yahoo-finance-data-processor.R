library(dplyr)

colnames(tmp)<-c("symbol","price","lastTradeDate","lastTradeTime","daysHigh","daysLow","volume","yearHigh","yearLow","open","prvClose")

# File Location where the dump is to be saved, If a file already exists we load the existing data and merge the new data to it and write back to this location
existingDataLocation <- "/code/CSCIE-107/E107project/pulkit/yahoo-finance.RData"
# File location for the zipped files, when the files are processed they are moved from this directory to a new location using  getDestinationFileName function
files <- list.files("/code/tot/Info/twitter-data/unprocessesed/",pattern = "^stocks.csv_",full.names = TRUE)

if(length(files)){
  for( j in 1:length(files)){
  }
}