library(tidyverse)
library(scales)
library(geojsonio)
library(broom)
library(mapproj)
set.seed(30)

airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "iso3166_2")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
state_airmen <- airmen %>% group_by(state) %>% slice_sample(n=1) %>% select(name)

centers <- centers %>% 
          left_join(., state_airmen, by=c("id"="state"))
          
centers$name <- str_wrap(centers$name, 8)

centers <- centers %>% 
          mutate(name = if_else(is.na(name), id, name))

plot_df <- airmen %>% 
              group_by(state) %>% 
              count() %>% 
              ungroup()

spdf_fortified <- spdf_fortified %>%
  left_join(. , plot_df, by=c("id"="state")) 


ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = n, x = long, y = lat, group = id) , size = 0.6, alpha=0.9, color = "black") +
  geom_text(data=centers, aes(x=x, y=y, label=name), color="black", size=3.5, alpha=0.6) +
  theme_void() +
  coord_map() +
  scale_fill_gradient(low = "white", high=muted("green")) +
  labs(title = "Members of the Tuskegee Airmen by state", fill = "Airmen") + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    plot.background = element_rect(fill = "transparent", color = NA), 
    panel.background = element_rect(fill = "transparent", color = NA), 
    legend.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) + 
  guides(fill = guide_colorbar( barwidth = unit(20, "lines"), 
                                barheight = unit(.3, "lines"),
                                title.position="top",
                                title.hjust = 0.5))
  


  