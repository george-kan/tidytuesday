library(tidyverse)
library(scales)
library(ggrepel)

theme_set(theme_minimal())

indoor_polution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')

#Keeping only countries
indoor_polution <- indoor_polution %>% 
                  filter(!is.na(Code) & Year %in% c(1990, 2019)) %>% 
                  select(-Code)

names(indoor_polution) <- c("Entity", "Year", "Deaths")                  

wide_data <- pivot_wider(indoor_polution, id_cols = c("Entity"), names_from = "Year", values_from = "Deaths") %>% 
                mutate(Difference = `2019` - `1990`) 
  

plot_data <- wide_data %>% 
                slice_min(Difference, n = 10) %>% 
                add_row(filter(wide_data, Entity == "World")) %>% 
                mutate(Entity_1990 = paste0(Entity, "(", round(`1990`,1), "%)"),
                       Entity_2019 = paste0(Entity, "(", round(`2019`,1), "%)"))



ggplot(plot_data) +
  geom_point(aes(y = `1990`, x = 0), color = "grey") + 
  geom_text_repel(aes(y = `1990`, x = 0, label = Entity_1990), nudge_x = -0.1, direction = "y", hjust = "right") + 
  geom_point(aes(y = `2019`, x = 1)) +
  geom_text_repel(aes(y = `2019`, x = 1, label = Entity_2019), nudge_x = 0.1, direction = "y", hjust = "left") +
  geom_segment(aes(y = `1990`, yend = `2019`, x = 0, xend = 1, size = Entity == "World", color = Entity == "World")) +
  scale_x_continuous(limits = c(-0.5, 1.5)) +
  scale_color_manual(guide = 'none', values = c("grey", "palegreen")) +
  scale_size_manual(values=c(0.6,1.3), guide = 'none') +
  annotate(geom="text", x = -0.05, y = -1.5, label =  "1990", size = 6) +
  annotate(geom="text", x = 1.05, y = -1.5, label =  "2019", size = 6) +
  labs(title = "Countries with the biggest \ndecrease in deaths related to indoor pollution") +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18))


