library(tidyverse)
library(scales)
library(ggrepel)
library(MetBrewer)
library(showtext)

font_add_google("Roboto Mono", "Roboto")
showtext_auto()


theme_set(theme_minimal(base_size = 16))
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

# Filtering out invalid dates
cran <- cran[grepl(".*(20[0-9]{2}).*", cran$date), ]
      
# Download the most popular packages
url <- "https://www.r-pkg.org/downloaded"
webpage <- read_html(url)

pop_packages <- html_nodes(webpage, "strong") %>% 
  html_text() %>% 
  str_trim() %>% 
  gsub(pattern = ".*\\t([^\\|\\s].*?)\\.*", replacement = "\\1")

pop_cran <- cran %>% 
            mutate(popular = package %in% pop_packages[1:10]) %>% 
            filter(popular == T) %>% 
            mutate(year = as.numeric(if_else(grepl("^(20[0-9]{2})", date),
                                  gsub("^(20[0-9]{2}).*", "\\1", date),
                                  gsub(".*(20[0-9]{2})$", "\\1", date))))

# Selecting the latest version of each package
plot_data <- pop_cran %>% 
                group_by(package) %>% 
                mutate(max_year = max(year), versions = n()) %>% 
                filter(year == max_year) %>% 
                slice(n()) %>% 
                ungroup() %>% 
                mutate(vignettes = rnw+rmd) %>% 
                select(package, vignettes, versions)

ggplot(plot_data, aes(y=vignettes, x=versions, color=package, label = package)) +
  geom_text_repel(size = 5, force = 0.4) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0,10,2)) +
  scale_color_manual(guide = 'none', values=met.brewer("Renoir", 10)) +
  labs(title = "Vignettes and verions of the most downloaded R packages",
       caption = "Popularity: https://www.r-pkg.org/downloaded") + 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom", 
        plot.title = element_text(family = "Roboto", hjust = 0.5, face = "plain", size = 15),
        plot.caption = element_text(color = "gray11", face = "italic", size = 9))




