library(twitteR)
library(ROAuth)
library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(stringr)
library(sentimentr)
library(stm)
library(geometry)
library(rsvd)
library(Rtsne)

clean.beer <- read_csv("AllFormattedTweets.csv")

#just want specific data as meta data --> data that would serve as a covariate: beer company and date 

beer.data <- clean.beer %>% 
  select(X1, text, screenName, weekday)

#process the data via STM textProcessor 
proc.beer <- textProcessor(documents = beer.data$text, metadata = beer.data)

out.put <- prepDocuments (proc.beer$documents, proc.beer$vocab, proc.beer$meta)
prep.docs <- out.put$documents 
prep.vocab <- out.put$vocab
prep.meta <- out.put$meta

#need to instal Rtsne, rsvd, and geometry

#from here, we can run the stm function for topic modelling 
beerModelFit <- stm(documents = prep.docs,
                    vocab = prep.vocab,
                    K =  40, #we can set k to pretty much anything 
                    prevalence =~ screenName + weekday,
                    data = prep.meta,
                    init.type = "Spectral",
                    seed=5674309) 

#Time to visualize the topics and see how much weekday influences the data. 

beertheta <- as.data.frame(beerModelFit$theta)

beertheta$topic <- apply(beertheta[,1:40], 1, which.max)
beertheta$topic.value <- apply(beertheta[, 1:40], 1, max)
beertheta$X1 <- 1:nrow(beertheta)

#take only variables of interest 
theta.clean <- beertheta %>% 
  select(X1, topic, topic.value)

#prepDoc deleted lines in beermeta, therefore X1 is not numbered correctly, have to delete and renumber, the rows 

beermeta <- within(prep.meta, rm(X1))

beermeta$X1 <- 1:nrow(prep.meta)

beermeta[8,] #X1 = 8, while row num = 9, but it is displayed correctly, X1 matches order correctly

#now that our beermeta's X1 is labeled correctly and corresponds with the actual number of the document, we can join the two datasets using X1 

beer.final.DOW <- prep.meta %>%
  left_join(theta.clean, by = "X1")

```

#Data visualization
```{r}
brand.topic.freq <- beer.final.DOW %>% 
  select(screenName, topic) %>%
  group_by(screenName) %>% 
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) 

brand.topic.freq
```

#creating a data frame for each day of the week for blue moon to see what the topics are for day of the week for one company.
```{r}
DOW.BM.M <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Mon") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.M 

DOW.BM.T <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Tues") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.T

DOW.BM.W <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Wed") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.W

DOW.BM.THUR <- 
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Thurs") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.THUR

DOW.BM.FRI <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Fri") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.FRI

DOW.BM.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.SAT

DOW.BM.SUN <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sun") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.SUN

#After looking at the days of the week, top five topics, blue moon generally tweets about the same 5 topics every day of the week. Since we are going to get a lot of bar plots with the same bar plots, we are going to compare the topics across our 5 companies on one day of the week. We will choose Saturday night.

#Creating a data frame just for saturday for each company to prep data for visualization

DOW.DE.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "DosEquis") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.DE.SAT


DOW.BL.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "budlight") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BL.SAT

DOW.TS.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "tsingtao") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.TS.SAT

DOW.GUS.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "GuinnessUS") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.GUS.SAT

```

#Barpolts of the top 5 most frequently tweeted topics on Saturdays by Company Name
```{r}
v = data.frame(topic=c('22','35','39','27','28'),n=c(72,59,39,27,27))
barplot(v$n, names.arg=y$topic, main = "Top 5 Tweeted topics on Saturdays by Blue Moon Beer Co.", ylim = c(0,80),ylab = "Count", xlab = "Topic", col=c("burlywood1","coral"))

w = data.frame(topic=c('22','11','25','3','37'),n=c(26,19,8,7,7))
barplot(w$n, names.arg=w$topic, main = "Top 5 Tweeted topics on Saturdays by Dos Equis Beer Co.", ylim = c(0,30),ylab = "Count", xlab = "Topic", col=c("burlywood1","coral"))


x = data.frame(topic=c('13','12','29','10','34'),n=c(26,19,8,7,7))
barplot(x$n, names.arg=x$topic, main = "Top 5 Tweeted topics on Saturdays by Budlight Beer Co.", ylim = c(0,30),ylab = "Count", xlab = "Topic",col=c("burlywood1","coral"))


y = data.frame(topic=c('8','22','4','7','28'),n=c(5,2,1,1,1))
barplot(y$n, names.arg=y$topic, main = "Top 5 Tweeted topics on Saturdays by Tsingtao Beer Co.", ylim = c(0,10),ylab = "Count", xlab = "Topic",col=c("burlywood1","coral") )


z = data.frame(topic=c('22','16','21','33','17'),n=c(66,39,10,4,3))
barplot(z$n, names.arg=z$topic, main = "Top 5 Tweeted topics on Saturdays by Guiness US Beer Co.", ylim = c(0,70),ylab = "Count", xlab = "Topic", col=c("burlywood1","coral"))


```