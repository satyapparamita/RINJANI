library(rtweet)
library(sentimentr)
library(dplyr)
library(syuzhet)
library(countrycode)

#load csv dataset
tweet1 <- read.csv("data.csv")
#keep variables

tweet1fix <- tweet1 %>% select(user_id,status_id,created_at,text,location,lat,lng)

#clean tweet
##create cleantweet function
catch.error = function(x){
  #testing with missing value
  y = NA
  #checking our NA
  catch_error = tryCatch(tolower(x), error=function(e) e)
  #if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  #check result if error exists, otherwise the function works fine
  return(y)
}
cleanTweets <- function(tweet) {
  #clean tweeets to do sentiment analysis
  #remove html links:
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  #remove retweet entities:
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  #remove #hashtags:
  tweet = gsub("#\\w+", " ", tweet)
  #remove all "@people":
  tweet = gsub("@\\w+", " ", tweet)
  #remove all punctuations:
  tweet = gsub("[[:punct:]]", " ", tweet)
  #remove numbers, kita hanya butuh teks untuk analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  #remove unnecessary spaces (white spaces, tabs, etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  #change to lowercase
  tweet = catch.error(tweet)
  tweet
}

#run clean tweet
tweet1fix$txtcln <- cleanTweets(tweet1fix$text)

#tweet1fix <- tweet1fix %>% distinct(txtcln,.keep_all = TRUE)#remove duplicate

#get sentiment score (neg/pos)
tweet1fix$sentimentscore <- sentiment(tweet1fix$txtcln,lexicon::hash_sentiment_huliu)

#get 8 axis emotion analysis
tweet1fix$nrc <- get_nrc_sentiment(tweet1fix$txtcln,language = "english")

#get state name
cd <- 'https://bit.ly/2ToSrFv'
cd <- read.csv(cd)
cd$state <- as.character(cd$state)
cd$abbreviation <- as.character(cd$abbreviation)
cd$state.regex <- as.character(cd$state.regex)
tweet1fix$state <- countrycode(tweet1fix$location,'state.regex','state',custom_dict = cd, origin_regex = TRUE)
tweet1fix$stateab <- countrycode(tweet1fix$location,'abbreviation','state',custom_dict = cd)