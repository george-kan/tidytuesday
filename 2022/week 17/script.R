library(tidyverse)
library(scales)
library(wordcloud2)
library(tm)

theme_set(theme_minimal(base_size = 22))

hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv')

text <- hidden_gems$review
docs <- Corpus(VectorSource(text))


docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("english"), "within", "also"))


dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


wordcloud2(data=df, size=1.6, color='random-dark')
