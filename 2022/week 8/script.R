library(tidyverse)
library(scales)
library(rnaturalearth)
theme_set(theme_minimal())

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

freedom['freedom_score'] <- 0.5*(100-(freedom$PR - 1)*100/6) + 0.5*(100-(freedom$CL - 1)*100/6)

freedom['country'] <- gsub("(.*?)\\s\\(.*", "\\1", freedom$country)


freedom <- freedom %>% 
              mutate(country =
                case_when(
                  country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                  country == "Viet Nam" ~ "Vietnam",
                  country == "Republic of Korea" ~ "South Korea",
                  country == "Russian Federation" ~ "Russia",
                  country == "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire" ~ "Ivory Coast",
                  country == "Czechia" ~ "Czech Republic",
                  country == "Syrian Arab Republic" ~ "Syria",
                  TRUE ~ country
                ))



plot_df <- freedom %>% 
  filter(year == 2020) %>% 
  select(country, year,  freedom_score)


sf_world <- ne_countries(scale = 10, returnclass = "sf")


map_plot_df <- sf_world %>% 
                  left_join(plot_df, by = c("admin" = "country"))


ggplot(map_plot_df) + 
  geom_sf(aes(fill = freedom_score)) +
  coord_sf(
    crs = '+proj=moll'
  ) + 
  scale_fill_gradient(low = "black", high = "cornflowerblue") +
  labs(title = 'Country <b style="color:#6495ED;">freedom</b> score for 2020',
       caption = "Freedom score is an assessment of civil \nliberties and political rights in a country") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = ggtext::element_markdown(size = 22, hjust =0.5),
        plot.caption = element_text(face = "italic")) +
  guides(fill = guide_colorbar(title.hjust = .5, 
                               barwidth = unit(20, "lines"), 
                               barheight = unit(.3, "lines")))


