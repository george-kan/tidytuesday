library(tidyverse)
library(ggforce)
library(ggrepel)

starbucks = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')
theme_set(theme_minimal() +
            theme(axis.title = element_text(size = 18),
                  axis.text = element_text(size = 14)))

starbucks %<>% 
  group_by(product_name) %>% 
  mutate(max_serv_size = max(serv_size_m_l), max_cal = max(calories))


plot_df = starbucks %>% 
          filter(serv_size_m_l == max_serv_size & calories == max_cal) %>% 
          group_by(product_name) %>% 
          slice(1) %>% 
          filter(size != "1 scoop")

plot_df %<>%
  mutate(label = if_else((between(sugar_g, 25, 50) & caffeine_mg > 250) | (caffeine_mg > 200 & sugar_g > 75), product_name, ""))

  
star_plot = ggplot(plot_df, aes(y = caffeine_mg, x = sugar_g )) +
                geom_point(aes(color = calories), size = 3) +
                geom_text_repel(aes(label = label), nudge_x = .15,
                                box.padding = 0.5,
                                nudge_y = 1,
                                segment.curvature = -0.1,
                                segment.size = 0.2,
                                segment.ncp = 0.6,
                                segment.angle = 10) +
                scale_color_gradient2(low="palegreen2", mid = "grey", high = "red", midpoint = 300) +
                geom_mark_rect(aes(filter = caffeine_mg > 400, label = "Brewed coffee corner"), alpha=0, show.legend = F, color = "brown") +
                labs(x = "Sugar(g)", y = "Caffeine(mg)", color = "Calories", title = "Caffeine-sugar content of large Starbucks beverages") +
                guides(color = guide_bins()) +
                theme(plot.title = element_text(face = "plain", hjust = 0.5, size = 20))


