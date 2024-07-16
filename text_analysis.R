
install.packages(c("stringr", "tidyverse", "tidytext", "reshape2","topicmodels"))

library(stringr)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(pdftools)
library(dplyr)
library(tidytext)
library(textdata)
library(topicmodels)
library(reshape2)

setwd('C:/Users/Gabmiato/Desktop/Interpelacje')
text <- pdf_text('Tekst/Interpelacje_sam_tekst.pdf')

#combine txt
combined_text <- paste(text, collapse=" ")

docs <- VCorpus(VectorSource(combined_text))

#cleaning
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, stripWhitespace)

# Polish stopwords, unfortunately no good library for that
docs <- tm_map(docs, removeWords, 'Szanowny Panie Ministrze!')
docs <- tm_map(docs, removeWords, 'Zgłaszający:')
docs <- tm_map(docs, removeWords, 'Data wpływu:')
docs <- tm_map(docs, removeWords, 'Jarosław Sachajko')
docs <- tm_map(docs, removeWords, 'Jarosław')
docs <- tm_map(docs, removeWords, 'Sachajko')
docs <- tm_map(docs, removeWords, 'jest')
docs <- tm_map(docs, removeWords, 'w sprawie')
docs <- tm_map(docs, removeWords, 'oraz')
docs <- tm_map(docs, removeWords, 'przez')
docs <- tm_map(docs, removeWords, 'dla')
docs <- tm_map(docs, removeWords, 'art.')
docs <- tm_map(docs, removeWords, 'się')
docs <- tm_map(docs, removeWords, 'czy')
docs <- tm_map(docs, removeWords, 'jak')
docs <- tm_map(docs, removeWords, 'lub')
docs <- tm_map(docs, removeWords, 'tej')
docs <- tm_map(docs, removeWords, 'się')
docs <- tm_map(docs, removeWords, 'czy')
docs <- tm_map(docs, removeWords, 'przy')
docs <- tm_map(docs, removeWords, 'nie')
docs <- tm_map(docs, removeWords, 'ale')

df_docs <- data.frame(id = sapply(docs, function(x) x$meta$id), 
                     text = sapply(docs, function(x) x$content),
                     stringsAsFactors = F)

#tokenize
word_tokens <- df_docs %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, '\\d')) %>% 
  filter(nchar(word) > 1)
word_tokens

'''
#analiza sentymentu z nrc
load('sentiments_lexicons.Rdata')

nrc %>% filter(sentiment %in% c('positive', 'negative'))

nrc %>% 
  filter(sentiment %in% c('positive', 'negative')) %>% 
  count(sentiment)

nrc_polarity <- nrc %>% 
  filter(sentiment %in% c('positive', 'negative'))

word_sent2 <- word_tokens %>% 
  inner_join(nrc_polarity, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 

word_sent_top <- word_sent2 %>% 
  group_by(sentiment) %>% 
  top_n(20, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

sent_plot1 <- ggplot(word_sent_top, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_grid(~sentiment, scales = 'free_x') +
  labs(x = "Words", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
sent_plot1

#tutaj inny pomysł na przedstawienie tego wykresu

sent_plot2 <- ggplot(word_sent_top, aes(x = word, y = n, fill = sentiment)) +
  geom_bar(stat='identity', position='identity') +
  scale_fill_manual(values = c("negative" = "firebrick2", "positive" = "olivedrab3")) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ sentiment, scales = 'free_y', space = "free") +
  scale_y_continuous(labels = abs, breaks = seq(-max(word_sent_top$n), max(word_sent_top$n), by = 20)) +
  labs(x = "Frequency", y = "Words") +
  theme(plot.title = element_text(hjust = 0.5))
sent_plot2
'''

#stemming
docs_stem <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
dtm_stem <- TermDocumentMatrix(docs_stem)

#wordcloud
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(words = d$word, 
          freq = d$freq, 
          min.freq = 3,
          max.words=40, 
          random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(9, "Dark2"))

#stemming-graph
m_stem <- as.matrix(dtm_stem)
v_stem <- sort(rowSums(m_stem),decreasing=TRUE)
d_stem <- data.frame(word = names(v_stem),freq=v_stem)
stem_plot <- d_stem %>%
  arrange(desc(freq)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "indianred") +
  labs(title = "Most frequent stems", x = "Stem", y = "Frequency") +
  coord_flip()
stem_plot

#top_words
top_plot1 <- d %>%
  arrange(desc(freq)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "paleturquoise3") +
  labs(title = "Najczęściej występujące słowa", x = "Słowo", y = "Częstotliwość") +
  coord_flip()
top_plot1

#top_words2
top_plot2 <- d %>%
  arrange(desc(freq)) %>%
  head(10) %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(x = word, y = freq)) +
  geom_segment(aes(xend = word, yend = 0), color = "indianred") +
  geom_point(size = 3, color = "indianred") +
  labs(title = "Najczęściej występujące słowa", x = "Słowo", y = "Częstotliwość") +
  coord_flip()
top_plot2

