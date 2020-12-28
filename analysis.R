#libraries
library(rtweet)
library(tidyverse)

#Get as many tweets as possible
df <- get_timeline("MBuhari", n = 3200)

#Write to csv
df %>% 
  write_as_csv("buhari.csv")


#read in data
df <- read.csv("buhari.csv")

#data dimension
dim(df)

#range of tweet collected
range(df$created_at)


#@load in datetime libraries
library(lubridate)
library(zoo)

#Lets take a look at his tweet trend over the years
 df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>%
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
  mutate(yearmon = as.yearmon(yearmon)) %>% 
  group_by(yearmon) %>% 
  count() %>% 
# x <- df2$yearmon
# lastDate <- max(df2$yearmon)
# class(lastDate)
# lastDateY <- df2[x==lastDate,2]
# lastDateY
  ggplot(., aes(yearmon, n, group = 1)) +
  geom_line(size = 1, color = "#00ba38") +
  geom_point(color = "#f8766d") +
  geom_smooth(method = "lm", se = F) +
  annotate(geom = "label", vjust = -0.1, 
           label="Humanitarian \ Crisis",x = as.yearmon("Dec 2016"), 
           y= 154, color = "black", size =3) +
  annotate(geom = "label", vjust = -0.1, 
           label="Covid'19",x = as.yearmon("Apr 2020"), 
           y= 189, color = "black", size =3) +
  annotate(geom = "label", vjust = -0.1, 
           label="Corruption",x = as.yearmon("Sep 2018"), 
           y= 163, color = "black", size =3)  +
  annotate(geom = "label", vjust = -0.1, 
           label="Football/Corruption/Tragedy",x = as.yearmon("Oct 2019"), 
           y= 157, color = "black", size =3) +
  annotate(geom = "label", vjust = -0.1, 
           label="Polio",x = as.yearmon("Jun 2020"), 
           y= 154, color = "black", size =3) +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 20)) +
  scale_x_yearmon(format = "%b %Y", n = 10) +
  labs(title = "Buhari's Tweet Trend",
       subtitle = "Sept2016 - Nov2020")


ggsave("test.png", dpi = 400)


#Investigate dec 2016
  df_dec2016<- df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>%
    mutate(date = ymd(date)) %>% 
    mutate(year = year(date)) %>% 
    mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
    mutate(yearmon = as.yearmon(yearmon)) %>% 
    filter(yearmon %in% as.yearmon("Jun 2020")) #%>% 
  
  df_dec2016 %>% 
    write.csv("2018tweet.csv")
  
    mutate(text = gsub("brt", "", text)) %>% 
    mutate(text = gsub("t.co", "", text)) %>% 
    mutate(text = gsub("https", "", text)) %>% 
    mutate(text = gsub("amp", "", text))
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    group_by(year) %>% 
    count(word, sort = T) %>% 
    arrange(desc(n)) %>% 
    mutate(word = reorder(word, n)) %>% 
    slice(1:10) %>% 
    ungroup() %>% 
    ggplot(., aes(word, n, fill = as.character(year))) +
    geom_col(show.legend = F) +
    coord_flip()

    
#text analysis  
library(scales)
library(tidytext)

    
#Find the most used word
df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  mutate(text = gsub("amp", "", text)) %>% 
  mutate(text = gsub("â", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  ggplot(., aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title =  "Top 20 most used words", x = "Words")
  
ggsave("test2.png", dpi = 400)  

#Most used words for each year
df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  mutate(text = gsub("amp", "", text)) %>% 
  mutate(text = gsub("â", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  group_by(year) %>% 
  count(word, sort = T) %>% 
  arrange(desc(n)) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  ggplot(., aes(reorder_within (word, n, year), n, fill = as.character(year))) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~year, ncol =  2, scales = "free") +
  labs(title = "Top 10 Words (2016 - 2020)", y = "Words")


library(tidytext)

#Bigram Analysis 
df %>% 
  as.data.frame() %>% 
  mutate(text = unlist(text)) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = F) %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  count(bigram, sort = TRUE) %>% 
  ungroup() %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  group_by(year) %>% 
  count(word1, word2, sort = TRUE) %>% 
  drop_na() %>% 
  ungroup() %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  group_by(year) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot(., aes(bigram, n, fill =as.character(year))) +
  geom_col(show.legend = F) +
  facet_wrap(~year, ncol = 2, scales = "free") +
  coord_flip()
  



#How about a trigram
df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3, collapse = F) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE) %>% 
  ungroup() %>% 
  drop_na() %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  group_by(year) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(trigram = reorder(trigram, n)) %>% 
  ggplot(., aes(trigram, n, fill = year)) +
  geom_col(show.legend = F) +
  facet_wrap(~year, ncol = 2, scales = "free") +
  coord_flip()
  


#Sentiment Analysis
df %>% 
inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()





#Tweets with security in it
df[grepl("security", tolower(df$text)),] %>%
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  group_by(date) %>% 
  count() %>% 
  mutate(date = ymd(date)) %>% 
  ggplot(., aes(date, log(n))) +
  geom_line(size = 1)



#Tweet by hour of the day
df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(time = hms(time)) %>% 
  mutate(hour = lubridate::hour(time)) %>% 
  group_by(hour) %>% 
  count() %>% 
  ggplot(., aes(factor(hour), n)) +
  geom_bar(stat = "identity") +
  labs( x = "Hour", title = "Tweet by Hour of Day")


df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(time = hms(time)) %>% 
  mutate(hour = lubridate::hour(time)) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  group_by(hour, year) %>%
  count() %>% 
  ggplot(., aes(factor(hour), n, fill = as.character(year))) +
  geom_col(show.legend = F) +
  facet_wrap(~year, ncol = 2, scales = "free")


#ggraph
library(ggraph)
library(igraph)
library(tidytext)

a <- df %>% 
  as.data.frame() %>% 
  mutate(text = unlist(text)) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = F) %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  count(bigram, sort = TRUE) %>% 
  ungroup() %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  group_by(year) %>% 
  count(word1, word2, sort = TRUE) %>% 
  ungroup() %>% 
  drop_na() %>% 
  arrange(desc(n)) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  group_by(bigram) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  top_n(40) %>% 
  graph_from_data_frame() %>% 
  ggraph(., layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 0.6, hjust = 0.2) +
  theme_void() 

a

ggsave("test9.jpeg", dpi = 400)

#Sentiments 
df %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ggplot(., aes(reorder(word, n), n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  coord_flip() +
  labs(x =  "Words")



#Most liked tweet
top_fav <- df %>%
  arrange(desc(favorite_count)) %>% 
  slice(1:10)


#Most Retweeted
top_rt <- df %>% 
  arrange(desc(retweet_count)) %>% 
  slice(1:10)


#Hourly tweet Trend
df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(time = hms(time)) %>% 
  mutate(hour = lubridate::hour(time)) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  group_by(hour, year) %>%
  count() %>% 
  ggplot(., aes(factor(hour), n, fill = as.character(year))) +
  geom_col(show.legend = F) +
  facet_wrap(~year, ncol = 2, scales = "free") +
  ggtitle("Tweets Count by Hour of Day") +
  xlab("Hour")


#Yearly trend of sentiments
dataInput <- df
data_format <- dataInput %>% 
  mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(row_id = row_number()) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment, row_id, screen_name) %>% 
  spread(sentiment, n, fill = 0) %>%  # made data wide rather than narrow
  mutate(sentiment = positive - negative)

All_banks <- dataInput %>%
  mutate(row_id = row_number())

data_format <- data_format %>% 
  left_join(All_banks, by = "row_id")
# 
# label_wrap <- label_wrap_gen(width = 60)
# 
# data_format2 <- data_format %>% 
#   rename(screenName = screenName.x) %>% 
#   select(-screenName.y) %>% 
#   mutate(text_formatted = str_glue("Row ID: {row_id}
#                                    Screen Name: {screenName}
#                                    Text: {label_wrap(text)} "))
# 
# data_format3<- data_format2 %>% 
#   select(1:5, "text_formatted")
# 
# 
# ggplotly(data_format3 %>% 
#            ggplot(aes(row_id, sentiment)) +
#            geom_line(color= "black", alpha = 0.5) +
#            geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
#            geom_hline(aes(yintercept = mean(sentiment), color = "blue")) +
#            geom_point(aes(text = text_formatted), color = "orange", shape = 5) +
#            geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "orange") +
#            geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "orange") +
#            theme_bw() +
#            labs(y = "sentiment score", x = "Twitter User"),
#          tooltip = "text") %>% 
#   layout(
#     xaxis = list(
#       rangeslider = list(type = "date")
#     )
#   )


# data_format %>% 
#   separate(created_at, into = c("date", "time"), sep = " ") %>%
#   mutate(date = ymd(date)) %>% 
#   mutate(year = year(date)) %>% 
#   mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
#   mutate(yearmon = as.yearmon(yearmon)) %>% 
#   group_by(yearmon) %>% 
#   summarise(sentiment = sum(sentiment)) %>% 
#   ggplot(., aes(yearmon, sentiment)) +
#   geom_line(size = 1, color = "#00ba38") +
#   geom_point(color = "#f8766d") +
#   geom_smooth(method = "lm", se = F)
# 


#convert negative column to negative values
data_format2<- data_format %>% 
  mutate(negative = negative * (-1))


data_format2 %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>%
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
  mutate(yearmon = as.yearmon(yearmon)) %>% 
  group_by(yearmon) %>% 
  summarise(negative = sum(negative), positive = sum(positive)) %>% 
  gather(key = "sentiment", value = "n", 2:3) %>% 
  ggplot(., aes(yearmon, n, group = sentiment, color = sentiment)) +
  geom_line(size = 0.8) +
  labs(title = "Sentiment Trend", x = "YearMonth", subtitle = "2016-2020") +
  theme_minimal() +
  scale_x_yearmon(format = "%b %Y", n = 10) +
  theme(axis.text.x = element_text(angle = 30))



ggsave("test8.jpeg", dpi = 400)  
  
  
