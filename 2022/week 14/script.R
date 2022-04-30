library(tidyverse)
library(scales)
library(emoGG)
library(showtext)

font_add_google("Roboto Mono", "Roboto")
showtext_auto()


theme_set(theme_minimal(base_size = 20))

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

news_orgs <- news_orgs %>% 
              filter(!is.na(state)) %>% 
              mutate(printed = grepl("print", tolower(distribution))) 

news_orgs <- news_orgs %>% 
                group_by(state) %>% 
                mutate(publications = n(), 
                       full_state = paste0(state, "(", n(), ")"))



ggplot() + 
  geom_jitter(data  = news_orgs  %>% filter(printed == F), 
              aes(x = year_founded, y = reorder(factor(full_state), publications)), 
              height=0, alpha = 0.4, size = 4) +
  geom_emoji(data = news_orgs %>% filter(printed == T), aes(x = year_founded, y = reorder(factor(full_state), -publications)), emoji = "1f4f0", size = 0.02) +
  annotate(geom = "text", x = 1966, y = 16.5, label = "Charlotte News is the oldest\n publication in the data, \nfounded in 1958", size = 5) +
  geom_curve(aes(x = 1965, y = 18, xend = 1958.5, yend = 20.5), size = 1.2, color = "gray20", curvature = -0.3, arrow = arrow(length = unit(0.07, "inch"))) +
  labs(title = "Publications by state", x = "Year founded") +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(family = "Roboto"),
        plot.title = element_text(family = "Roboto", hjust = 0.5),
        panel.grid.major.x = element_blank())



