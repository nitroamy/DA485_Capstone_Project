#push dataframes saved as csv format 
daily_tw
write.csv(daily_tw,"dailyTwCount.csv", row.names = FALSE)


#single word by week
w1 <- data.frame(c(rep("wk1",time=nrow(word_count_w1))), word_count_w1)
colnames(w1) <- c("week","word","n")
w2 <- data.frame(c(rep("wk2",time=nrow(word_count_w2))), word_count_w2)
colnames(w2) <- c("week","word","n")
w3 <- data.frame(c(rep("wk3",time=nrow(word_count_w3))), word_count_w3)
colnames(w3) <- c("week","word","n")
w4 <- data.frame(c(rep("wk4",time=nrow(word_count_w4))), word_count_w4)
colnames(w4) <- c("week","word","n")
w5 <- data.frame(c(rep("wk5",time=nrow(word_count_w5))), word_count_w5)
colnames(w5) <- c("week","word","n")
w6 <- data.frame(c(rep("wk6",time=nrow(word_count_w6))), word_count_w6)
colnames(w6) <- c("week","word","n")
w7 <- data.frame(c(rep("wk7",time=nrow(word_count_w7))), word_count_w7)
colnames(w7) <- c("week","word","n")
w8 <- data.frame(c(rep("wk8",time=nrow(word_count_w8))), word_count_w8)
colnames(w8) <- c("week","word","n")
w9 <- data.frame(c(rep("wk9",time=nrow(word_count_w9))), word_count_w9)
colnames(w9) <- c("week","word","n")
w10 <- data.frame(c(rep("wk10",time=nrow(word_count_w10))), word_count_w10)
colnames(w10) <- c("week","word","n")
w11 <- data.frame(c(rep("wk11",time=nrow(word_count_w11))), word_count_w11)
colnames(w11) <- c("week","word","n")
w12 <- data.frame(c(rep("wk12",time=nrow(word_count_w12))), word_count_w12)
colnames(w12) <- c("week","word","n")
w13 <- data.frame(c(rep("wk13",time=nrow(word_count_w13))), word_count_w13)
colnames(w13) <- c("week","word","n")
word_count_master <- rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13)
write.csv(word_count_master,"oneWrod.csv", row.names = FALSE)

#bigram_n
write.csv(bigram_n,"bigram.csv", row.names = FALSE)
#trigram_count
write.csv(trigram_n,"trigram.csv", row.names = FALSE)


# count sentiment
count_bing <- bing_tweet %>% 
  count(sentiment)
count_nrc <- nrc_tweet %>%
  count(sentiment)
bing_nrc <- rbind(count_bing,count_nrc)
write.csv(bing_nrc,"sentiment.csv", row.names = FALSE)


#weekly output
bing_w1 <- wk1_bing_tweet %>%
  count(sentiment)
nrc_w1 <- wk1_nrc_tweet %>%
  count(sentiment)
b_w1 <- data.frame(c(rep("wk1",time=2)), bing_w1)
colnames(b_w1) <- c("week", "sentiment", "n")
n_w1 <- data.frame(c(rep("wk1",time=8)), nrc_w1)
colnames(n_w1) <- c("week", "sentiment", "n")

bing_w2 <- wk2_bing_tweet %>%
  count(sentiment)
nrc_w2 <- wk2_nrc_tweet %>%
  count(sentiment)
b_w2 <- data.frame(c(rep("wk2",time=2)), bing_w2)
colnames(b_w2) <- c("week", "sentiment", "n")
n_w2 <- data.frame(c(rep("wk2",time=8)), nrc_w2)
colnames(n_w2) <- c("week", "sentiment", "n")

bing_w3 <- wk3_bing_tweet %>%
  count(sentiment)
nrc_w3 <- wk3_nrc_tweet %>%
  count(sentiment)
b_w3 <- data.frame(c(rep("wk3",time=2)), bing_w3)
colnames(b_w3) <- c("week", "sentiment", "n")
n_w3 <- data.frame(c(rep("wk3",time=8)), nrc_w3)
colnames(n_w3) <- c("week", "sentiment", "n")

bing_w4 <- wk4_bing_tweet %>%
  count(sentiment)
nrc_w4 <- wk4_nrc_tweet %>%
  count(sentiment)
b_w4 <- data.frame(c(rep("wk4",time=2)), bing_w4)
colnames(b_w4) <- c("week", "sentiment", "n")
n_w4 <- data.frame(c(rep("wk4",time=8)), nrc_w4)
colnames(n_w4) <- c("week", "sentiment", "n")

bing_w5 <- wk5_bing_tweet %>%
  count(sentiment)
nrc_w5 <- wk5_nrc_tweet %>%
  count(sentiment)
b_w5 <- data.frame(c(rep("wk5",time=2)), bing_w5)
colnames(b_w5) <- c("week", "sentiment", "n")
n_w5 <- data.frame(c(rep("wk5",time=8)), nrc_w5)
colnames(n_w5) <- c("week", "sentiment", "n")

bing_w6 <- wk6_bing_tweet %>%
  count(sentiment)
nrc_w6 <- wk6_nrc_tweet %>%
  count(sentiment)
b_w6 <- data.frame(c(rep("wk6",time=2)), bing_w6)
colnames(b_w6) <- c("week", "sentiment", "n")
n_w6 <- data.frame(c(rep("wk6",time=8)), nrc_w6)
colnames(n_w6) <- c("week", "sentiment", "n")

bing_w7 <- wk7_bing_tweet %>%
  count(sentiment)
nrc_w7 <- wk7_nrc_tweet %>%
  count(sentiment)
b_w7 <- data.frame(c(rep("wk7",time=2)), bing_w7)
colnames(b_w7) <- c("week", "sentiment", "n")
n_w7 <- data.frame(c(rep("wk7",time=8)), nrc_w7)
colnames(n_w7) <- c("week", "sentiment", "n")

bing_w8 <- wk8_bing_tweet %>%
  count(sentiment)
nrc_w8 <- wk8_nrc_tweet %>%
  count(sentiment)
b_w8 <- data.frame(c(rep("wk8",time=2)), bing_w8)
colnames(b_w8) <- c("week", "sentiment", "n")
n_w8 <- data.frame(c(rep("wk8",time=8)), nrc_w8)
colnames(n_w8) <- c("week", "sentiment", "n")

bing_w9 <- wk9_bing_tweet %>%
  count(sentiment)
nrc_w9 <- wk9_nrc_tweet %>%
  count(sentiment)
b_w9 <- data.frame(c(rep("wk9",time=2)), bing_w9)
colnames(b_w9) <- c("week", "sentiment", "n")
n_w9 <- data.frame(c(rep("wk9",time=8)), nrc_w9)
colnames(n_w9) <- c("week", "sentiment", "n")

bing_w10 <- wk10_bing_tweet %>%
  count(sentiment)
nrc_w10 <- wk10_nrc_tweet %>%
  count(sentiment)
b_w10 <- data.frame(c(rep("wk10",time=2)), bing_w10)
colnames(b_w10) <- c("week", "sentiment", "n")
n_w10 <- data.frame(c(rep("wk10",time=8)), nrc_w10)
colnames(n_w10) <- c("week", "sentiment", "n")

bing_w11 <- wk11_bing_tweet %>%
  count(sentiment)
nrc_w11 <- wk11_nrc_tweet %>%
  count(sentiment)
b_w11 <- data.frame(c(rep("wk11",time=2)), bing_w11)
colnames(b_w11) <- c("week", "sentiment", "n")
n_w11 <- data.frame(c(rep("wk11",time=8)), nrc_w11)
colnames(n_w11) <- c("week", "sentiment", "n")

bing_w12 <- wk12_bing_tweet %>%
  count(sentiment)
nrc_w12 <- wk12_nrc_tweet %>%
  count(sentiment)
b_w12 <- data.frame(c(rep("wk12",time=2)), bing_w12)
colnames(b_w12) <- c("week", "sentiment", "n")
n_w12 <- data.frame(c(rep("wk12",time=8)), nrc_w12)
colnames(n_w12) <- c("week", "sentiment", "n")

bing_w13 <- wk13_bing_tweet %>%
  count(sentiment)
nrc_w13 <- wk13_nrc_tweet %>%
  count(sentiment)
b_w13 <- data.frame(c(rep("wk13",time=2)), bing_w13)
colnames(b_w13) <- c("week", "sentiment", "n")
n_w13 <- data.frame(c(rep("wk13",time=8)), nrc_w13)
colnames(n_w13) <- c("week", "sentiment", "n")

bing_master <- rbind(b_w1,b_w2,b_w3,b_w4,b_w5,b_w6,b_w7,b_w8,b_w9,b_w10,b_w11,b_w12,b_w13)
nrc_master <- rbind(n_w1,n_w2,n_w3,n_w4,n_w5,n_w6,n_w7,n_w8,n_w9,n_w10,n_w11,n_w12,n_w13)

write.csv(bing_master,"bingSentiment.csv", row.names = FALSE)
write.csv(nrc_master,"nrcSentiment.csv", row.names = FALSE)

#-------------------------------------
#word frequency
freq_word <- word_count %>%
  count(word, sort = TRUE)
freq_word

total <- word_count %>% 
  summarize(total = sum(n))
freq_word <- data.frame(word_count, total)
freq_word$freq <- freq_word$n/freq_word$total

total2 <- bigram_n %>% 
  summarize(total2 = sum(n))
freq_bigram <- data.frame(bigram_n,total2)
freq_bigram$freq <- freq_bigram$n/freq_bigram$total2
freq_bigram

total3 <- bigram_n %>% 
  summarize(total3 = sum(n))
freq_trigram <- data.frame(trigram_n,total3)
freq_trigram$freq <- freq_trigram$n/freq_trigram$total3
freq_trigram


#---------------------------------------
word_count_master
big
tri
word_count
bigram_com
trigram_com


word_count %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n,fill = -n)) +
  geom_col() +
  guides(fill = FALSE)+
  labs(x = NULL, y = "Count") +
  ggtitle("Top 10 Words, single word") +
  coord_flip()

bigram_n %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = -n)) +
  geom_col() +
  guides(fill = FALSE)+
  labs(x = NULL, y = "Count") +
  ggtitle("Top 10 Words, bigram") +
  coord_flip()

trigram_n %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n,fill = -n)) +
  geom_col() +
  guides(fill = FALSE)+
  labs(x = NULL, y = "Count") +
  ggtitle("Top 10 Words, trigram") +
  coord_flip()



#tf_idf
week_word <- word_count_master %>%
  bind_tf_idf(word, week, n) %>%
  arrange(tf_idf,desc(n))

week_big <- big %>%
  bind_tf_idf(bigram, week, n) %>%
  arrange(tf_idf,desc(n))

week_tri <- tri %>%
  bind_tf_idf(bigram, week, n) %>%
  arrange(tf_idf,desc(n)) 

write.csv(week_word,"tfidf_word.csv", row.names = FALSE)
write.csv(week_big,"tfidf_bigram.csv", row.names = FALSE)
write.csv(week_tri,"tfidf_trigram.csv", row.names = FALSE)


