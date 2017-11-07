library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(rebus)
library(tidytext)
tweets<-read.csv("AllFormattedTweets.csv")

tweets$date<-as.Date(tweets$date.stamp)
tweets$yearmon<-format(tweets$date,"%Y-%m")

tweets <- tweets %>%
  group_by(yearmon) %>%
  filter(retweetCount==max(retweetCount)) %>%
  select(text, screenName,yearmon,retweetCount)

# Tokenize text into words and remove stop words
tweets$text<-as.character(tweets$text)
tweets <-tweets %>%
  unnest_tokens(word,text) 
clean_tweets <- anti_join(tweets,stop_words,by="word")
clean_tweets <- clean_tweets %>%
  filter(word != "de")
top_tweets <-clean_tweets %>%
  group_by(word) %>%
  count() %>%
  ungroup() %>%
  top_n(10) %>%
  arrange(desc(n))

#ggplot
ggplot(top_tweets) +
  geom_bar(mapping = aes(x=reorder(word,n),y=n),stat="identity")+xlab("words") +ylab("Frequency") + coord_flip()+
  ggtitle("Common words among tweets with highest retweets monthly") + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),                plot.title = element_text(size = 11,face="bold"))