library(tidyverse)
library(scales)
library(wordcloud2)

theme_set(theme_minimal())

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

characteristics <- chocolate %>% 
                      separate_rows(most_memorable_characteristics, sep = ",") %>% 
                      select(most_memorable_characteristics)
  
characteristics <- characteristics %>% 
                      mutate(word = str_trim(most_memorable_characteristics))

wc_df <- characteristics %>% 
            count(word) %>% 
            filter(n > 1 & word != "") %>% 
            rename(freq = n)

wordcloud2(wc_df, backgroundColor="#4F311C")

