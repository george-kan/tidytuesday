library(tidyverse)
library(scales)
library(ggrepel)
library(ggnewscale)

theme_set(theme_minimal(base_size = 28))

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


fuel_gdp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_gdp.csv')

#Keeping only countries
gdp <- fuel_gdp %>% 
              filter(!is.na(Code) & Year %in% c(1990, 2019)) %>% 
              select(Entity, Year, `GDP per capita, PPP (constant 2017 international $)`) %>% 
              group_by(Entity) %>% 
              filter(sum(!is.na(`GDP per capita, PPP (constant 2017 international $)`)) == 2) %>% 
              ungroup() %>% 
              rename(`GDP per capita` = `GDP per capita, PPP (constant 2017 international $)`)


wide_gdp <- pivot_wider(gdp, id_cols = c("Entity"), names_from = "Year", values_from = "GDP per capita") %>% 
    mutate(Perc_change = (`2019` - `1990`)/ `1990`) %>% 
    top_n(30, abs(Perc_change))


plot_data <- plot_data %>% 
                mutate(Top_30_gdp = Entity %in% wide_gdp$Entity, 
                       No_gdp_comp = !Entity %in% gdp$Entity,
                       Entity_1990 = if_else(No_gdp_comp == T, paste0(Entity_1990, "*"), Entity_1990)) 


ggplot(plot_data) +
  geom_point(aes(y = `1990`, x = 0), color = "grey") + 
  geom_text_repel(aes(y = `1990`, x = 0, label = Entity_1990, color = Top_30_gdp), nudge_x = -0.1, direction = "y", hjust = "right") + 
  geom_point(aes(y = `2019`, x = 1), color = "grey") +
  geom_text_repel(aes(y = `2019`, x = 1, label = Entity_2019, color = Top_30_gdp), nudge_x = 0.1, direction = "y", hjust = "left") +
  scale_color_manual(guide = 'none', values = c("black", "blue")) +
  scale_x_continuous(limits = c(-0.5, 1.5)) +
  scale_size_manual(values=c(0.6,1.3), guide = 'none') +
  new_scale_color() +
  geom_segment(aes(y = `1990`, yend = `2019`, x = 0, xend = 1, size = Entity == "World", color = Entity == "World")) +
  scale_color_manual(guide = 'none', values = c("grey", "palegreen")) +
  annotate(geom="text", x = -0.05, y = -3, label =  "1990", size = 6) +
  annotate(geom="text", x = 1.05, y = -3, label =  "2019", size = 6) +
  labs(title = "Countries with the biggest \ndecrease in deaths related to indoor pollution", 
       caption = "Country in the top 30 of GDP per capita increase\n*No GDP data for Maldives & Yemen") +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.caption = element_text(hjust = 0, color = "blue", size = 12, margin = margin(0,0,0,0)))

