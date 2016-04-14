#install.packages("jsonlite")
library(jsonlite)

read_extract <- function(filename) {
  
  con  <- file(filename, open = "r")
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
  hashtags <-c(NA)
  df_hashtags <- data.frame(id_str=NA, tags=NA)
  df_symbols <- data.frame(id_str=NA,symbols=NA)
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    tryCatch({
    json_data <- fromJSON(oneLine)
    
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
    
    time_stamp <- c(time_stamp,ifelse(is.null(json_data$timestamp_ms),NA,json_data$timestamp_ms))
    retweeted <-c(retweeted,ifelse(is.null(json_data$retweeted),NA,json_data$retweeted))
    retweeted_count <- c(retweeted_count,ifelse(is.null(json_data$retweet_count),NA,json_data$retweet_count))
    if(is.vector(json_data$entities) & is.list(json_data$entities)) {
      if(is.data.frame(json_data$entities$symbols)) {
        df_symbols <- rbind(df_symbols,invisible(data.frame(id_str=tmpuser_id,symbols = json_data$entities$symbols$text)))
       # symbols <- c(symbols,c(tmpuser_id,list(json_data$entities$symbols$text)))
      }
      if(is.data.frame(json_data$entities$hashtags)) {
        df_hashtags <- rbind(df_hashtags,invisible(data.frame(id_str=tmpuser_id,tags = json_data$entities$hashtags$text)))
        
       # hashtags <- c(hashtags,c(tmpuser_id,list(json_data$entities$hashtags$text)))
      }
    }
    }, warning = function(war) {
      print(paste("Warning:  ",err))
    }, error = function(err) {
      print(paste("ERROR:  ",err))
    })
    
  } 
  
  close(con)
  tweets<-data.frame(id_str,text,reply_to,reply_usr_id,user_id,time_stamp,retweeted,retweeted_count)
  users <- data.frame(user_id,user_name,user_loc,user_follower_count,user_friend_count,user_profile_image)
  
  tweets <- tweets %>% filter(!is.na(id_str))
  users <- users %>% filter(!is.na(user_id))
  tweet_hashtags <- df_hashtags %>% filter( !is.na(id_str) & !is.na(tags))
  tweet_symbols <-df_symbols %>% filter( !is.na(id_str) & !is.na(symbols))
  
  list(tweets=tweets,users=users,tweet_hashtags=tweet_hashtags,tweet_symbols= tweet_symbols)
  #write.table(df, filename, rownames = FALSE)  # don't quote FALSE
}

files <- list.files("/code/tot/Info/twitter-data/unprocessesed/",pattern = "^twitter.json",full.names = TRUE)
results <- invisible(lapply(files, read_extract))

View(results[[4]]$tweets)
View(results[[1]]$users)
str(results[[1]]$tweet_hashtags)                  
str(results[[1]]$tweet_symbols)                  
