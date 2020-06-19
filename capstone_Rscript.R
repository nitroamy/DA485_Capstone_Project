#https://www.tidytextmining.com/ngrams.html
#https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25
#https://www.tidytextmining.com/tfidf.html
#https://www.tidytextmining.com/topicmodeling.html

library(XLConnect)
library(dplyr)
library(tidyr)
library(tidytext)
library(textcat)
library(ggplot2)
library(RColorBrewer)

#load data
tw1 <- read.csv("tw1.csv", header=TRUE)
tw2 <- read.csv("tw2.csv", header=TRUE)
tw3 <- read.csv("tw3.csv", header=TRUE)
tw4 <- read.csv("tw4.csv", header=TRUE)
tw5 <- read.csv("tw5.csv", header=TRUE)

tw_raw.df <- rbind(tw1,tw2,tw3,tw4,tw5)
tweet.df <- tw_raw.df[,c(2,3,6,7,8)]
colnames(tweet.df) <- c("PostDate","Tweet","CountRT","CountLike","TweetValue")

# dataframe to remove any symbols, non-english characters
tweet.df$clean_text <- gsub("http\\S+", " ", tweet.df$Tweet)
tweet.df$clean_text <- gsub("@\\w+", " ", tweet.df$clean_text)
tweet.df$clean_text <- gsub("[^\x01-\x7F]", " ", tweet.df$clean_text)
tweet.df$clean_text <- gsub("[\r\n]", " ", tweet.df$clean_text)
tweet.df$clean_text <- gsub('[0-9]+', " " , tweet.df$clean_text)

#detect language in order to remove non-english tweet
tweet.df$lang <- textcat(tweet.df$clean_text)

# new dataframe with only english tweets
en_tweet <- filter(tweet.df, lang=="english")

# date format d/m/y to m/d/y
en_tweet$PostDate <- as.Date(en_tweet$PostDate, format="%d/%m/%Y")

#order tweets by date
en_tweet <- en_tweet[order(en_tweet$PostDate),]

#add week column
en_tweet$week <- en_tweet %>% mutate(case_when(PostDate < as.Date("2020-1-29") ~ "wk1",
                                               PostDate >= as.Date("2020-1-29") & PostDate < as.Date("2020-2-5") ~ "wk2",
                                               PostDate >= as.Date("2020-2-5") & PostDate < as.Date("2020-2-12") ~ "wk3",
                                               PostDate >= as.Date("2020-2-12") & PostDate < as.Date("2020-2-19") ~ "wk4",
                                               PostDate >= as.Date("2020-2-19") & PostDate < as.Date("2020-2-26") ~ "wk5",
                                               PostDate >= as.Date("2020-2-26") & PostDate < as.Date("2020-3-4") ~ "wk6",
                                               PostDate >= as.Date("2020-3-4") & PostDate < as.Date("2020-3-11") ~ "wk7",
                                               PostDate >= as.Date("2020-3-11") & PostDate < as.Date("2020-3-18") ~ "wk8",
                                               PostDate >= as.Date("2020-3-18") & PostDate < as.Date("2020-3-25") ~ "wk9",
                                               PostDate >= as.Date("2020-3-25") & PostDate < as.Date("2020-4-1") ~ "wk10",
                                               PostDate >= as.Date("2020-4-1") & PostDate < as.Date("2020-4-8") ~ "wk11",
                                               PostDate >= as.Date("2020-4-8") & PostDate < as.Date("2020-4-15") ~ "wk12",
                                               PostDate >= as.Date("2020-4-15") & PostDate < as.Date("2020-4-23") ~ "wk13"))


tweet.v1 <- en_tweet$week
colnames(tweet.v1) <- c("PostDate","Tweet","CountRT","CountLike","TweetVal","CleanText","Lang","week")
tweet.v2 <- tweet.v1[,c(1,8,6,3,4,5)]

#subsetting by week 
week1 <- subset(tweet.v2, week=="wk1")
week2 <- subset(tweet.v2, week=="wk2")
week3 <- subset(tweet.v2, week=="wk3")
week4 <- subset(tweet.v2, week=="wk4")
week5 <- subset(tweet.v2, week=="wk5")
week6 <- subset(tweet.v2, week=="wk6")
week7 <- subset(tweet.v2, week=="wk7")
week8 <- subset(tweet.v2, week=="wk8")
week9 <- subset(tweet.v2, week=="wk9")
week10 <- subset(tweet.v2, week=="wk10")
week11 <- subset(tweet.v2, week=="wk11")
week12 <- subset(tweet.v2, week=="wk12")
week13 <- subset(tweet.v2, week=="wk13")

#--------------------------------------------------------------------------------#
#most retweet
top_twt <- tweet.v2 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk1 <- week1 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk2 <- week2 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk3 <- week3 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk4 <- week4 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk5 <- week5 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk6 <- week6 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk7 <- week7 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk8 <- week8 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk9 <- week9 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk10 <- week10 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk11 <- week11 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk12 <- week12 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
top_twt_wk13 <- week13 %>%
  arrange(desc(CountRT)) %>%
  top_n(10)
#-------------------------------------------------------------------------------#

# Entire set: Jan 22, 2020 to Apr 22, 2020
#bigram
bigram <- tweet.v2 %>%
  unnest_tokens(bigram, CleanText, token = "ngrams", n=2) %>%
  separate(bigram, c("word1","word2"), sep=" ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word)

bigram_count <- bigram %>%  
  count(word1, word2, sort = TRUE)

bigram_com <- bigram %>%
  unite(bigram, word1, word2, sep=" ")

bigram_n <- bigram_com %>%
  count(bigram) %>%
#  bind_tf_idf(bigram, week, n) %>%
  arrange(desc(n))
bigram_n

bigram_wn <- bigram_com %>%
  count(week,bigram) %>%
#  bind_tf_idf(bigram, week, n) %>%
  arrange(desc(n))
bigram_wn

#weekly bigram
bigram_wk1 <- subset(bigram_wn, week=="wk1")
bigram_wk2 <- subset(bigram_wn, week=="wk2")
bigram_wk3 <- subset(bigram_wn, week=="wk3")
bigram_wk4 <- subset(bigram_wn, week=="wk4")
bigram_wk5 <- subset(bigram_wn, week=="wk5")
bigram_wk6 <- subset(bigram_wn, week=="wk6")
bigram_wk7 <- subset(bigram_wn, week=="wk7")
bigram_wk8 <- subset(bigram_wn, week=="wk8")
bigram_wk9 <- subset(bigram_wn, week=="wk9")
bigram_wk10 <- subset(bigram_wn, week=="wk10")
bigram_wk11 <- subset(bigram_wn, week=="wk11")
bigram_wk12 <- subset(bigram_wn, week=="wk12")
bigram_wk13 <- subset(bigram_wn, week=="wk13")

# trigram
trigram <- tweet.v2 %>%
  unnest_tokens(bigram, CleanText, token = "ngrams", n=3) %>%
  separate(bigram, c("word1","word2","word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word)

trigram_count <- trigram %>%
  count(word1, word2, word3, sort = TRUE)

trigram_com <- trigram %>%
  unite(bigram, word1, word2, word3, sep=" ")

trigram_n <- trigram_com %>%
  count(bigram) %>%
#  bind_tf_idf(bigram, week, n) %>%
  arrange(desc(n))
trigram_n

trigram_wn <- trigram_com %>%
  count(week,bigram) %>%
  #  bind_tf_idf(bigram, week, n) %>%
  arrange(desc(n))
trigram_wn

#weekly trigram
trigram_wk1 <- subset(trigram_wn, week=="wk1")
trigram_wk2 <- subset(trigram_wn, week=="wk2")
trigram_wk3 <- subset(trigram_wn, week=="wk3")
trigram_wk4 <- subset(trigram_wn, week=="wk4")
trigram_wk5 <- subset(trigram_wn, week=="wk5")
trigram_wk6 <- subset(trigram_wn, week=="wk6")
trigram_wk7 <- subset(trigram_wn, week=="wk7")
trigram_wk8 <- subset(trigram_wn, week=="wk8")
trigram_wk9 <- subset(trigram_wn, week=="wk9")
trigram_wk10 <- subset(trigram_wn, week=="wk10")
trigram_wk11 <- subset(trigram_wn, week=="wk11")
trigram_wk12 <- subset(trigram_wn, week=="wk12")
trigram_wk13 <- subset(trigram_wn, week=="wk13")


#---------------------------------------------------------------------------------------------------------#

# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
tweet.stem <- tweet.v2 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
tweet.clean <- tweet.stem %>%
  anti_join(stop_words)
word_count <- tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
bing_tweet = tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
bing_tweet
#count number
bing_tweet %>%
  count(sentiment) %>%
  arrange(desc(n))

#Polarity plot
polar_tweet <- bing_tweet %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = c("blue","red"))) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Covid-19 Polarity") +
  coord_flip()
polar_tweet

# nrc sentiment analysis
#sentiment plot
nrc_tweet = tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
nrc_tweet
#count number
nrc_tweet %>%
  count(sentiment) %>%
  arrange(desc(n))

#plot
sentiment_plot <- nrc_tweet %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("COVID-19 NRC Sentiment") +
  coord_flip()
sentiment_plot

#-----------------------------------------------------------------------------------------------
#week1 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk1_tweet.stem <- week1 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk1_tweet.clean <- wk1_tweet.stem %>%
  anti_join(stop_words)
word_count_w1 <- wk1_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk1_bing_tweet = wk1_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk1_bing_tweet

# nrc sentiment analysis
wk1_nrc_tweet = wk1_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk1_nrc_tweet

#week2 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk2_tweet.stem <- week2 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk2_tweet.clean <- wk2_tweet.stem %>%
  anti_join(stop_words)
word_count_w2 <- wk2_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk2_bing_tweet = wk2_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk2_bing_tweet

# nrc sentiment analysis
wk2_nrc_tweet = wk2_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk2_nrc_tweet

#week3 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk3_tweet.stem <- week3 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk3_tweet.clean <- wk3_tweet.stem %>%
  anti_join(stop_words)
word_count_w3 <- wk3_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk3_bing_tweet = wk3_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk1_bing_tweet

# nrc sentiment analysis
wk3_nrc_tweet = wk3_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk3_nrc_tweet

#week4 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk4_tweet.stem <- week4 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk4_tweet.clean <- wk4_tweet.stem %>%
  anti_join(stop_words)
word_count_w4 <- wk4_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk4_bing_tweet = wk4_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk4_bing_tweet

# nrc sentiment analysis
wk4_nrc_tweet = wk4_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk4_nrc_tweet

#week5 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk5_tweet.stem <- week5 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk5_tweet.clean <- wk5_tweet.stem %>%
  anti_join(stop_words)
word_count_w5 <- wk5_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk5_bing_tweet = wk5_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk5_bing_tweet

# nrc sentiment analysis
wk5_nrc_tweet = wk5_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk5_nrc_tweet

#week6 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk6_tweet.stem <- week6 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk6_tweet.clean <- wk6_tweet.stem %>%
  anti_join(stop_words)
word_count_w6 <- wk6_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk6_bing_tweet = wk6_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk6_bing_tweet

# nrc sentiment analysis
wk6_nrc_tweet = wk6_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk6_nrc_tweet

#week7 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk7_tweet.stem <- week7 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk7_tweet.clean <- wk7_tweet.stem %>%
  anti_join(stop_words)
word_count_w7 <- wk7_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk7_bing_tweet = wk7_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk7_bing_tweet

# nrc sentiment analysis
wk7_nrc_tweet = wk7_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk7_nrc_tweet

#week8 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk8_tweet.stem <- week8 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk8_tweet.clean <- wk8_tweet.stem %>%
  anti_join(stop_words)
word_count_w8 <- wk8_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk8_bing_tweet = wk8_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk8_bing_tweet

# nrc sentiment analysis
wk8_nrc_tweet = wk8_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk8_nrc_tweet

#week9 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk9_tweet.stem <- week9 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk9_tweet.clean <- wk9_tweet.stem %>%
  anti_join(stop_words)
word_count_w9 <- wk9_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk9_bing_tweet = wk9_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk9_bing_tweet

# nrc sentiment analysis
wk9_nrc_tweet = wk9_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk9_nrc_tweet

#week10 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk10_tweet.stem <- week10 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk10_tweet.clean <- wk10_tweet.stem %>%
  anti_join(stop_words)
word_count_w10 <- wk10_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk10_bing_tweet = wk10_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk10_bing_tweet

# nrc sentiment analysis
wk10_nrc_tweet = wk10_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk10_nrc_tweet

#week11 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk11_tweet.stem <- week11 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk11_tweet.clean <- wk11_tweet.stem %>%
  anti_join(stop_words)
word_count_w11 <- wk11_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk11_bing_tweet = wk11_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk11_bing_tweet

# nrc sentiment analysis
wk11_nrc_tweet = wk11_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk11_nrc_tweet

#week12 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk12_tweet.stem <- week12 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk12_tweet.clean <- wk12_tweet.stem %>%
  anti_join(stop_words)
word_count_w12 <- wk12_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk12_bing_tweet = wk12_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk12_bing_tweet

# nrc sentiment analysis
wk12_nrc_tweet = wk12_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk12_nrc_tweet

#week13 analysis
# bing polarity analysis
#unnext_tokens() function to convert to lowercase, remove punctuation
wk13_tweet.stem <- week13 %>%
  select(CleanText) %>%
  unnest_tokens(word, CleanText)
#remove stop words
wk13_tweet.clean <- wk13_tweet.stem %>%
  anti_join(stop_words)
word_count_w13 <- wk13_tweet.clean %>%
  count(word) %>%
  arrange(desc(n))

#bing sentiment analysis
wk13_bing_tweet = wk13_tweet.clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk13_bing_tweet

# nrc sentiment analysis
wk13_nrc_tweet = wk13_tweet.clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
wk13_nrc_tweet

#------------------------------------------------------------------------------------
# graph count data
daily_tw <- tweet.v2 %>%
  count(PostDate)

plot(daily_tw, type="l", main="Daily Tweet Count", ylab="Number of unique tweets", xlab="Post Date")


