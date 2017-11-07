##Check which libraries need to be installed
check_packages <- function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}

check_packages("dplyr")
check_packages("tidytext")
check_packages("lubridate")
check_packages("stringr")
check_packages("stringi")
check_packages("DataCombine")
check_packages("rebus")
check_packages("syuzhet")
check_packages("ggplot2")

##Twitter Extraction Libraries
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(stringi)
library(DataCombine)
library(rebus)
library(syuzhet)
library(ggplot2)

##Load Data from Beer Tweets Data Wrangling RMD file
##Because of private credentials, Data Wrangling file is
AllBeerTweets <- read.csv("AllFormattedTweets.csv")

##Sentiment Analysis From package syuzhet
TweetsSent <- AllBeerTweets %>% 
  mutate(reformattedtext = as.character(reformattedtext)) %>% 
  mutate(sentiment = get_sentiment(reformattedtext, method = "syuzhet")) %>% 
  mutate(sentimentnrc = get_sentiment(reformattedtext, method = "nrc"))

head(TweetsSent)


table(TweetsSent$sentiment)

testdf <- TweetsSent %>% 
  group_by(screenName, weekday) %>% 
  summarise(Mean = mean(sentiment)) %>% 
  arrange(screenName, weekday)

levels(testdf$weekday) <- c( "Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")

sentimentplot <- ggplot(testdf, aes(weekday, Mean)) 

sentimentplot +
  geom_point() +
  facet_wrap(~screenName)

##Sentiment Analysis From  Data Camp
# Access bing lexicon: bing
bing <- get_sentiments("bing")
# Access afinn lexicon: afinn
afinn <- get_sentiments("afinn")
# Access nrc lexicon: nrc
nrc <- get_sentiments("nrc")
# Access loughran lexicon: loughran
loughran <- get_sentiments("loughran")

testdata <- as.data.frame(TweetsSent[16,]$reformattedtext)
head(testdata)

testdata <- testdata %>% 
  rename(word = `TweetsSent[16, ]$reformattedtext`) %>% 
  mutate(word = as.character(word))

head(testdata)

testdata %>% 
  mutate(word2 = str_match_all( %in% nrc$word)
         
         
         inner_join(nrc)
         
         
         
         for(i in 1:length(testdata)) {
           testdata <- as.data.frame(TweetsSent[i,]$reformattedtext)
           head(testdata)
           
           testdata <- testdata %>% 
             rename(word = `TweetsSent[1, ]$reformattedtext`) %>% 
             mutate(word = as.character(word))
           
           head(testdata)
           
           testdata %>% 
             inner_join(nrc)
         }
         
         as.data.frame(testdata[1:2]) %>% 
           inner_join(nrc)
         
         AllBeerTweets[1:6,] %>% 
           mutate(tweetWords = list(AllBeerTweets$reformattedtext))  
         
         ifelse(AllBeerTweets$reformattedtext %in% nrc$word))


TweetsText <- as.data.frame(AllBeerTweets$reformattedtext)
# Use data frame with text data
TweetsText %>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(bing)


# tweets_nrc has been pre-defined
tweets_nrc

joy_words <- tweets_nrc %>%
  # Filter to choose only words associated with joy
  filter(sentiment == "joy") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))



joy_words %>%
  top_n(20) %>%
  mutate(word = reorder(word, freq)) %>%
  # Use aes() to put words on the x-axis and frequency on the y-axis
  ggplot(aes(word, freq)) +
  # Make a bar chart with geom_col()
  geom_col()+
  coord_flip() 
