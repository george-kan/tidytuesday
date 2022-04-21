library(tidyverse)
library(scales)
library(tidytext)

theme_set(theme_minimal(base_size = 22))

big_dave <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')


common_answers <- rbind(big_dave %>% 
                          filter(!is.na(answer)) %>% 
                          group_by(answer) %>% 
                          count() %>% 
                          ungroup() %>% 
                          slice_max(n = 10, order_by = n) %>% 
                          mutate(source = "Big dave (source)") ,
                        
                        times %>% 
                          filter(answer != "D") %>% 
                          group_by(answer) %>% 
                          count() %>% 
                          ungroup() %>% 
                          slice_max(n = 10, order_by = n) %>% 
                          mutate(source = "Times (source)"))


popular_letters <- names(sort(table(unlist(strsplit(common_answers$answer, ""))), decreasing = T)[1:4])

ann_text <- data.frame(source = "Times (source)", label = "All words contain\none of:\nE,L,R or N", 
                       answer = "EAR", n = 74)


p1 <- ggplot(common_answers, aes(x = n, y = reorder_within(answer, n, source), fill = source)) +
          geom_col(show.legend = F) +
          facet_wrap(source ~ ., scales = "free_y") +
          scale_y_reordered() + 
          scale_fill_manual(values = c("darkgreen", "lightblue")) +
          labs(title = "Most popular answers in crosswords", x = "Crosswords") +
          theme(axis.title.y = element_blank(),
                panel.grid = element_blank(),
                plot.title = element_text(hjust = 0.5))

p1 + geom_text(data = ann_text, aes(label = label), size = 7)
