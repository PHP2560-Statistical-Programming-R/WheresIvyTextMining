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

#Density Plot
sentHist <- ggplot(TweetsSent, aes(x = sentiment, fill = "red"))
sentHist + geom_density(position = "identity", alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "none") + 
  ggtitle("Density of Sentiment Scores for all beer companies") + 
  xlab("Sentiment Score") + ylab("Density")

#Individual Density Plot
sentHistI <- ggplot(TweetsSent, aes(x = sentiment, fill = "green"))

sentHistI + geom_density(position = "identity", alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "none") + 
  ggtitle("Density of Sentiment Scores for individual beer companies") + 
  xlab("Sentiment Score") + ylab("Density") +
  facet_wrap(~screenName)

#Summarise Average Sentiment Scores
testdf <- TweetsSent %>% 
  group_by(screenName, weekday) %>% 
  summarise(Mean = mean(sentiment, na.rm = T)) %>% 
  arrange(screenName, weekday)

#Relevel the days of the week to progress towards weekend
levels(testdf$weekday) <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")

#Individual Sentiment Plot per weekday
sentimentplot <- ggplot(testdf, aes(weekday, Mean)) 

sentimentplot +
  geom_point() +
  facet_wrap(~screenName) + 
  theme_bw() +
  theme(legend.position = "none") + 
  ggtitle("Average Sentiment Score as the week progresses to the weekend") + 
  ylab("Average Sentiment Score") + xlab("Day of the Week") +
  facet_wrap(~screenName)




