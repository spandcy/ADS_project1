library(tidyverse)
library(tidytext)
library(DT)
library(scales)
library(wordcloud2)
library(gridExtra)
library(ngram)
library(dplyr)
library(janeaustenr)
library(tidytext)
library(SnowballC)
library(wordcloud)
library(koRpus)



hm_data <- read.csv("processed_moments.csv")
#hm_data <- processed_moments
urlfile<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/demographic.csv'
demo_data <- read.csv(urlfile)
hm_data <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         text) %>%
  mutate(count = sapply(hm_data$text, wordcount)) %>%
  filter(gender %in% c("m", "f")) %>%
  filter(marital %in% c("single", "married")) %>%
  filter(parenthood %in% c("n", "y")) %>%
  filter(reflection_period %in% c("24h", "3m")) %>%
  mutate(reflection_period = fct_recode(reflection_period, 
                                        months_3 = "3m", hours_24 = "24h"))

bag_of_words <-  hm_data %>%
  unnest_tokens(word, text)

word_count <- bag_of_words %>%
  count(wid,word,gender,marital,parenthood,reflection_period,country, sort = TRUE)



wordcloud(word_count$word, word_count$n, max.words=100, colors=brewer.pal(1, "Dark2"))
#Someone might argue that in this word cloud we have day, night, morning that take no meaning and we have words 'bought' and 'buy'
#appear in the word cloud but they mean the same. Therefore we firstly need to first do the lemmatization to avoid different past tense
#and then we need to use tf-idf to eliminate the common words like 'morning' and etc.


wordcloud(phrase_counts$bigram, phrase_counts$n, max.words=50, colors=brewer.pal(1, "Dark2"))

library(textstem)
bag_of_words$word=lemmatize_words(bag_of_words$word)
word_count_lemm <- bag_of_words %>%
  count(word, sort = TRUE)

library(tm)

hm_corpus <- VCorpus(VectorSource(hm_data$original_hm))
hm_corpus = tm_map(hm_corpus, content_transformer(tolower))
hm_corpus = tm_map(hm_corpus, removeNumbers)
hm_corpus = tm_map(hm_corpus, removePunctuation)
hm_corpus = tm_map(hm_corpus, removeWords, c("the", "and",'day', stopwords("english")))
hm_corpus =  tm_map(hm_corpus, stripWhitespace)
  

hm_tfidf <- DocumentTermMatrix(hm_corpus, control = list(weighting = weightTfIdf))
hm_tfidf = removeSparseTerms(hm_tfidf, 0.99)

freq = data.frame(sort(colSums(as.matrix(hm_tfidf)), decreasing=TRUE))
df=data.frame(lemm_words=lemmatize_words(rownames(freq)),score=freq[,1])
df_new=df[!duplicated(df$lemm_words), ]
wordcloud(df_new$lemm_words, df_new$score, max.words=100, colors=brewer.pal(3, "Dark2"))
#Now the word cloud emphasis looks better than the previously, 'friend' might be less relative important, while people prefer
#to tell their happy moment with past experience especially from yesterday. Then we need to analyze the time period influence people


hm_words_time <- word_count %>%
  bind_tf_idf(word, wid, n)
hm_words_time

hm_words_time <- hm_words_time[nchar(hm_words_time$word)>3,]

hm_words_time %>% 
  group_by(reflection_period) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~reflection_period, ncol = 2, scales = "free") +
  coord_flip()



#We notice in the happy moment there are some negative words like jail and etc. We analyze the sentiment by different age group here


young_words <-  hm_data %>%
  filter(age >=17 & age<30) %>%
  unnest_tokens(word, text)

mid_age_words <- hm_data %>%
  filter(age >=30 & age<60) %>%
  unnest_tokens(word, text)

senior_words <- hm_data %>%
  filter(age >60) %>%
  unnest_tokens(word, text)

age_stage <- bind_rows(young_words %>%
                         inner_join(get_sentiments("bing")) %>%
                         count(word, sentiment) %>%
                         spread(sentiment, n, fill = 0) %>%
                         mutate(sentiment = positive - negative,method="young_adult"),
                       mid_age_words %>%
                         inner_join(get_sentiments("bing")) %>%
                         count(word, sentiment) %>%
                         spread(sentiment, n, fill = 0) %>%
                         mutate(sentiment = positive - negative,method="mid_aged"),
                       senior_words %>%
                         inner_join(get_sentiments("bing")) %>%
                         count(word, sentiment) %>%
                         spread(sentiment, n, fill = 0) %>%
                         mutate(sentiment = positive - negative,method="senior"))


age_stage %>%
  ggplot(aes(word, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "sentiment")+
  facet_wrap(~method, ncol = 1, scales = "free_y")

#Here we notice that senior has extremely low sentiment score, because senior have much more experience than young adult and 
#mid-aged did. Therefore they have less motion violatility with their words.

ggplot(age_stage, aes(x = method, y = sentiment, color = method)) + 
  geom_boxplot() # draw a boxplot for each president

#By the boxplot we notice young adult have more extremley value than mid-aged for the positive and negative which make sense.


#Although the blog are all about happinese moments, we believe that their words will definitely contian negative words,
#some negative words will be overcomed or solved by another negative word to be a postive phrases, some negatvie moment are past tense
#and should be happened previously which make authors feel happy today.
library(reshape2)
bag_of_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray80", "gray20"),
                   max.words = 100)

plot()

#We finished sentiment analysis and then we would like to analysis the phrase and bigram
library(igraph)
library(ggraph)

hm_bigrams <- hm_data %>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_counts <- hm_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 100) %>%
  graph_from_data_frame()


set.seed(0)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#In bigram, we believe friend is the most important word which consist the bigram phrase


#Finally we want to figure out the topic analysis we use LDA method
library(topicmodels)


hm_lda_2 <- LDA(DocumentTermMatrix(hm_corpus), k = 2, control = list(seed = 1234))

#I firstly assign two topic to the happy moment corpus
text_topics <- tidy(hm_lda_2, matrix = "beta")

text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

text_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Then I try to find out the max probability difference between topic 1 and topic 2
beta_spread <- text_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(15, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()
#Maybe topic 1 is regarding eating or males and topic 2 is daily useage, but I did not see much difference and hard to recogize
#two topics boundary

#I try to split to 6 topics. Assuming they belong to gender, marital, partnerhood,reflection_period, age and country
hm_lda_6 <- LDA(DocumentTermMatrix(hm_corpus), k = 6, control = list(seed = 1234))
text_topics1 <- tidy(hm_lda_6, matrix = "beta")
text_top_terms1 <- text_topics1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

text_top_terms1 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
#Topic 3 or 5 might be parentnerhood and gender, while topic 6 should be marital, topic 1 might be related to age, but absolutely every topic is
#related to happinese, for parentnerhood, child might be their happinese, marital spouse will be their happinese, for mid-aged people
#working and daily males will be their happinese, for young adult, friends might be their primary happinese


