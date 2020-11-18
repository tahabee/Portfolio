install.packages('rtweet')
install.packages('tidytext')
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages('tm')
install.packages('slam')
install.packages('syuzhet')
library(wordcloud)
library(dplyr)
library(rtweet)
library(tidyr)
library(tidytext)
library(ggplot2)
library(syuzhet)

# Scraping twitter for tweets related to autonomous vehicles
av <- search_tweets('autonomous vehicles', n = 1000, include_rts = FALSE)

# Remove retweets
av_tweets_organic <- av[av$is_retweet==FALSE, ] 

# Remove replies
av_tweets_organic <- subset(av_tweets_organic, is.na(av_tweets_organic$reply_to_status_id))
av_tweets_organic <- av_tweets_organic %>% arrange(-favorite_count)
av_tweets_organic[1,5]
av_tweets_organic <- av_tweets_organic %>% arrange(-retweet_count)
av_tweets_organic[1,5]

av_tweets_organic$text <-  gsub("https\\S*", "", av_tweets_organic$text)
av_tweets_organic$text <-  gsub("@\\S*", "", av_tweets_organic$text) 
av_tweets_organic$text  <-  gsub("amp", "", av_tweets_organic$text) 
av_tweets_organic$text  <-  gsub("[\r\n]", "", av_tweets_organic$text)
av_tweets_organic$text  <-  gsub("[[:punct:]]", "", av_tweets_organic$text)

tweets <- av_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)

tweets %>% # bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the autonomous vehicle tweets",
       subtitle = "Stop words removed from the list")

av_tweets_organic$hashtags <- as.character(av_tweets_organic$hashtags)
av_tweets_organic$hashtags <- gsub("c\\(", "", av_tweets_organic$hashtags)
set.seed(1234)
wordcloud(av_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

#plotting total sentiment based on scores
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()


install.packages('sentimentr')
library(sentimentr)
get_nrc_sentiment(tweets)
av_tweets_organic$sentimentscore <- sentiment(av_tweets_organic$text)
av_tweets_organic$symbols <- as.matrix(av_tweets_organic$symbols)
av_tweets_organic$ur
write.csv(av_tweets_organic, '~/Downloads/sentiment.csv')
