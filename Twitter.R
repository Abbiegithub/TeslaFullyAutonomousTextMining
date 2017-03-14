install.packages("twitteR")
install.packages("RCurl")
require("twitteR")
require("RCurl")
consumer_key <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
consumer_secret <- 'xxxxxxxxxxxxxxxxxxxx'
consumer_token <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_secret <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
setup_twitter_oauth(consumer_key,consumer_secret,consumer_token,access_secret)
xyz <- searchTwitter("#tesla", n=30000, lang="en", since = "2016-10-01", until = "2016-12-06", resultType = "recent")
xyz1 <- searchTwitter("#Autonomous", n=30000, lang="en", since = "2016-10-01", until = "2016-12-06", resultType = "recent")
xyz2 <- searchTwitter("#Autopilot", n=30000, lang="en", since = "2016-10-01", until = "2016-12-06", resultType = "recent")
xyz3 <- searchTwitter("#Selfdriving", n=30000, lang="en", since = "2016-10-01", until = "2016-12-06", resultType = "recent")
xyz4 <- searchTwitter("#TeslaModelX", n=30000, lang="en", since = "2016-10-01", until = "2016-12-06", resultType = "recent")

tweetsDF <- twListToDF(xyz)
tweetsDF1 <- twListToDF(xyz1)
tweetsDF2 <- twListToDF(xyz2)
tweetsDF3 <- twListToDF(xyz3)
tweetsDF4 <- twListToDF(xyz4)

tweetsDFFin <- rbind.data.frame(tweetsDF,tweetsDF1,tweetsDF2,tweetsDF3,tweetsDF4, make.row.names = TRUE)
tweetsDFFin1 <- dplyr::select(tweetsDFFin,-created)


getLocation <- function(x) {
y <- getUser(x)
location <- y$location
}
tweetsDFFin$screenName
locations <- sapply(tweetsDFFin$screenName, function(x) getLocation(x))
locations

LocationDF <- data.frame(locations = unlist(locations))


TextPreprocessingLocation <- lapply(LocationDF, function(x) {
  
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  
  x = gsub("[^[:alnum:]]", " ", x) ## Remove non-alphanumerics
  
  x = gsub('\\b+RT', '', x) ## Remove RT
  
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  x = gsub('@\\S+', '', x) ## Remove Mentions
  
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  
  x = gsub(' +',' ',x) ## Remove extra whitespaces
  
})

LocationDF1 <- data.frame(TextPreprocessingLocation = unlist(TextPreprocessingLocation))

TextPreprocessing <- lapply(tweetsDFFin1, function(x) {
  

  x = gsub('http\\S+\\s*', '', x) ## Remove URLs

  x = gsub("[^[:alnum:]]", " ", x) ## Remove non-alphanumerics
  
  x = gsub('\\b+RT', '', x) ## Remove RT
  
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  x = gsub('@\\S+', '', x) ## Remove Mentions
  
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  
  x = gsub(' +',' ',x) ## Remove extra whitespaces
  
})
tweetsDFFin2 <- as.data.frame(tweetsDFFin$created)
TextPreprocessingDF <- as.data.frame(TextPreprocessing)
TextPreprocessing1 <- cbind(TextPreprocessingDF, tweetsDFFin2)
View(TextPreprocessing1)

library(qpcR)
combined <- qpcR:::cbind.na(TextPreprocessing1, LocationDF1)
combinedFinal<-distinct(combined, text) # remove duplicate tweets (load dplyr before executing)
View(combinedFinal)
write.csv(combinedFinal, file = "clean1.csv")
