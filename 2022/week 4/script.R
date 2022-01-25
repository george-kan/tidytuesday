library(tidyverse)
library(scales)
library(ggrepel)
library(MetBrewer)

theme_set(theme_minimal())

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

rated_games = ratings %>% 
                filter(users_rated > 1000) %>% 
                select(name, bayes_average) %>% 
                inner_join(details, by = c("name" = "primary"))
              
rated_games["decade"] = if_else(rated_games$yearpublished %/% 10 < 195, "Earlier games", paste0(rated_games$yearpublished %/% 10, "0s"))

top_3 = rated_games %>% 
          group_by(decade) %>% 
          slice_max(order_by= bayes_average, n=3, with_ties=F) %>% 
          select(name, bayes_average, yearpublished, playingtime, decade) %>% 
          ungroup()

top_3["name"] = if_else(grepl("Sherlock", top_3$name), "Sherlock Holmes Consulting Detective", top_3$name)
top_3 = top_3 %>% 
          mutate(rank = rank(yearpublished, ties.method = "random"))

decade_v =  c("Earlier\n games", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")        

 
ggplot(top_3, aes(y = rank, x = bayes_average, color = decade)) +
  geom_point() +
  geom_text(aes(label = name), check_overlap = TRUE,  hjust = -0.1, size = 4.3) +
  #geom_point() +
  scale_y_continuous(breaks = seq(2, 27, 3), labels = decade_v) +
  scale_x_continuous(limits = c(5, 10), breaks = c(5, 7, 9)) +
  scale_color_manual(values = met.brewer("Cross")) +
  labs(x = "Adjusted rating", title = "Top 3 board games by decade") + 
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "none",
        panel.grid.major = element_blank())

