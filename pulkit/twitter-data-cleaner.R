#install.packages("jsonlite")
library(jsonlite)
library(dplyr)

# File Location where the dump is to be saved, If a file already exists we load the existing data and merge the new data to it and write back to this location
existingTweetsDataLocation <- "/code/CSCIE-107/E107project/pulkit/twitter.RData"
# File location for the zipped files, when the files are processed they are moved from this directory to a new location using  getDestinationFileName function
files <- list.files("/code/tot/Info/twitter-data/unprocessesed/",pattern = "^twitter.json_",full.names = TRUE)

getDestinationFileName <- function(fileName, replaceWord="unprocessesed", withWord="processesed"){
  gsub(replaceWord, withWord, fileName)
}

# Function to create the directory path if the path doesnt exist
createDirectoryTree <- function(toFileName) {
  todir <- dirname(toFileName)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
}

read_extract <- function(filename) {
  cat(" Processing file",filename)
  con  <- gzfile(filename, open = "r")
  id_str<- c(NA)
  text <- c(NA)
  reply_to <- c(NA)
  reply_usr_id <- c(NA)
  user_id <- c(NA)
  user_name <- c(NA)
  user_loc <-c(NA)
  user_follower_count<-c(NA)
  user_friend_count <- c(NA)
  user_profile_image <- c(NA)
  time_stamp <- c(NA)
  symbols <- c(NA)
  retweeted <-c(NA)
  retweeted_count <- c(NA)
  retweeted_from_id <-c(NA)
  retweeted_from_count <- c(NA)
  hashtags <-c(NA)
  df_hashtags <- data.frame(id_str=NA, tags=NA)
  df_symbols <- data.frame(id_str=NA,symbols=NA)
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    tryCatch({
      json_data <- fromJSON(oneLine)
      if(!is.na(json_data$id_str) & !is.na(json_data$text)){
        id_str <- c(id_str,json_data$id_str)
        text <- c(text,json_data$text)
         
        reply_to <- c(reply_to,ifelse(is.null(json_data$in_reply_to_status_id_str),NA,json_data$in_reply_to_status_id_str))
        reply_usr_id <- c(reply_usr_id,ifelse(is.null(json_data$in_reply_to_user_id_str),NA,json_data$in_reply_to_user_id_str))
        tmpuser_id <- ifelse(is.null(json_data$user$id_str),NA,json_data$user$id_str)
        if(is.vector(json_data$user) & is.list(json_data$user)) { 
          user_id <- c(user_id,tmpuser_id)
          user_name <- c(user_name,ifelse(is.null(json_data$user$name),NA,json_data$user$name))
          user_loc <-c(user_loc,ifelse(is.null(json_data$user$location),NA,json_data$user$location))
          user_follower_count<-c(user_follower_count,ifelse(is.null(json_data$user$followers_count),NA,json_data$user$followers_count))
          user_friend_count <- c(user_friend_count,ifelse(is.null(json_data$user$friends_count),NA,json_data$user$friends_count))
          user_profile_image <- c(user_profile_image,ifelse(is.null(json_data$user$profile_image_url_https),NA,json_data$user$profile_image_url_https))
        }else{
          user_id <- c(user_id,NA)
          user_name <- c(user_name,NA)
          user_loc <-c(user_loc,NA)
          user_follower_count<-c(user_follower_count,NA)
          user_friend_count <- c(user_friend_count,NA)
          user_profile_image <- c(user_profile_image,NA)
        }
        if(is.vector(json_data$retweeted_status) & is.list(json_data$retweeted_status)) { 
          retweeted_from_id <- c(retweeted_from_id,json_data$retweeted_status$id_str)
          retweeted_from_count <- c(retweeted_from_count,json_data$retweeted_status$retweet_count)
        }else{
          retweeted_from_id <- c(retweeted_from_id,NA)
          retweeted_from_count <- c(retweeted_from_count,NA)
        }
        time_stamp <- c(time_stamp,ifelse(is.null(json_data$timestamp_ms),NA,json_data$timestamp_ms))
        retweeted <-c(retweeted,ifelse(is.null(json_data$retweeted),NA,json_data$retweeted))
        retweeted_count <- c(retweeted_count,ifelse(is.null(json_data$retweet_count),NA,json_data$retweet_count))
        if(is.vector(json_data$entities) & is.list(json_data$entities)) {
          if(is.data.frame(json_data$entities$symbols)) {
            df_symbols <- rbind(df_symbols,invisible(data.frame(id_str=json_data$id_str,symbols = json_data$entities$symbols$text)))
            # symbols <- c(symbols,c(tmpuser_id,list(json_data$entities$symbols$text)))
          }
          if(is.data.frame(json_data$entities$hashtags)) {
            df_hashtags <- rbind(df_hashtags,invisible(data.frame(id_str=json_data$id_str,tags = json_data$entities$hashtags$text)))
            
            # hashtags <- c(hashtags,c(tmpuser_id,list(json_data$entities$hashtags$text)))
          }
        }
      }
    }, warning = function(war) {
      print(paste("Warning:  ",war))
    }, error = function(err) {
      print(paste("ERROR:  ",err))
    })
    
  } 
  
  close(con)
  
  tweets<-data.frame(id_str,text,reply_to,reply_usr_id,user_id,time_stamp,retweeted,retweeted_count,retweeted_from_id,retweeted_from_count)
  users <- data.frame(user_id,user_name,user_loc,user_follower_count,user_friend_count,user_profile_image)
  
  tweets <- tweets %>% filter(!is.na(id_str))
  users <- users %>% filter(!is.na(user_id))
  tweet_hashtags <- df_hashtags %>% filter( !is.na(id_str) & !is.na(tags))
  tweet_symbols <-df_symbols %>% filter( !is.na(id_str) & !is.na(symbols))
  gc()
  list(tweets=tweets,users=users,tweet_hashtags=tweet_hashtags,tweet_symbols= tweet_symbols)
  #write.table(df, filename, rownames = FALSE)  # don't quote FALSE
}
# Apply the function read_extract on all the files that are currently not processesed
results <- invisible(lapply(files, read_extract))

# Check if there is an existing dump of the data
if(file.exists(existingTweetsDataLocation)){
  # Load the existing data, this will result in creation of dataframes for tweets, hashtags, symbols, users
  load(existingTweetsDataLocation)
}else{
  tweets<- data.frame(id_str=character(),text=character(),reply_to=character(),reply_usr_id=character(),user_id=character(),time_stamp=character(),
                    retweeted=logical(),retweeted_count=numeric(),retweeted_from_id=character(),retweeted_from_count=numeric(),
                    stringsAsFactors=FALSE) 
  users <- data.frame(user_id=character(),user_name=character(),user_loc=character(),user_follower_count=integer(),user_friend_count=integer(),user_profile_image=character(), stringsAsFactors=FALSE)
  hashtags <- data.frame(id_str=character(),tags=character(), stringsAsFactors=FALSE)
  symbols <- data.frame(id_str=character(),symbols=character(), stringsAsFactors=FALSE)
}
# Combine the data into the datasets
if(length(results) >0){
  for(i in 1:length(results))
  {
    tweets <- bind_rows(tweets,results[[i]]$tweets) 
    users <- bind_rows(users,results[[i]]$users) 
    hashtags <- bind_rows(hashtags,results[[i]]$tweet_hashtags) 
    symbols <- bind_rows(symbols,results[[i]]$tweet_symbols) 
  }
}
gc()
# remove duplicate users and select the last entry for them
users <- users[ !duplicated(users$user_id,fromLast=TRUE), ]
# Save the file to the same location so that it can be read back when next data is available
save(tweets, users,hashtags,symbols, file = existingTweetsDataLocation)

# Move the files as they have been processed successfully.
if(length(files)){
  for( j in 1:length(files)){
    newName <- getDestinationFileName(files[j])
    createDirectoryTree(newName)
    file.rename(files[j], newName)
    cat("Moving file", files[j], " to ",newName,"\n")
  }
}

