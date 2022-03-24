library(tidyverse)
library(scales)
library(showtext)
library(gganimate)

font_add_google("Roboto Mono", "Roboto")
showtext_auto()

theme_set(theme_minimal(base_size = 20))

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

top_10 <- babynames %>% 
            group_by(year) %>% 
            slice_max(order_by = prop, n=10) %>% 
            mutate(ranking = rank(-prop)) %>% 
            ungroup()


static <- ggplot(top_10, aes(x = prop/2, y = -ranking, fill = name, group = name)) +
                geom_tile(aes(height = 0.9, width = prop), alpha = 0.8, color = NA) +
                geom_text(aes(label = name, x = prop/2), size = 7, family="Roboto") + 
                scale_fill_discrete(guide = 'none') +
                scale_x_continuous(labels = percent) +
                scale_y_continuous(expand = c(0,0)) +
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      plot.title = element_text(family = "Roboto", hjust = 0.5),
                      axis.text.y = element_blank(),
                      axis.title.y = element_blank())

anim = static + 
          #transition_states(year, transition_length = 3, state_length = 1) +
          transition_time(year) +
          view_follow(fixed_x = T) +
          ease_aes('cubic-in-out') +
          labs(title = 'Most popular baby names for: {round(frame_time)}',
               x = "Percent of total babies")

animate(anim,  nframes = 600, fps = 8, width = 1200, height = 1000, end_pause = 10, start_pause = 10)

anim_save("Rplot.gif", animation = last_animation())
