library(tidyverse)
library(scales)
library(broom)
set.seed(30)

library(showtext)
#options("device" = "windows")
font_add_google("Public Sans", "Psans")
showtext_auto()

theme_set(theme_minimal(base_size = 20))

input_df <- readr::read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge05/data.csv')
input_df <- input_df %>% 
              mutate(Slave = 3-Free)


input_df <- input_df %>% 
  mutate(
    Slave = if_else(Year == 1870, 3, Slave),
    Free = if_else(Year == 1870, 0, Free),
  )

segm_df <- input_df %>% 
              select(Year) %>% 
              mutate(start = 0,
                     end = 3)

value_df <- input_df %>% 
              select(Year, Free) %>% 
              mutate(value = case_when(
                          Year == 1790 ~ paste0(Free, "%"),
                          Year == 1870 ~ "100%",
                          TRUE ~ as.character(Free))
              )

input_long <- pivot_longer(input_df, c("Slave", "Free"), names_to = "status")

black_df <- input_long %>% 
              filter(status == "Slave") %>% 
              mutate(prev_val = lead(value)) %>% 
              filter(!is.na(prev_val))


black_df <- black_df %>%
              mutate(freq = 4) %>% 
              uncount(freq) %>% 
              group_by(Year) %>% 
              mutate(row_num = row_number()) %>% 
              filter(Year < 1860)

black_df <- black_df %>%
              mutate(
                value = case_when(
                  row_num %in% c(1,4) ~ 0,
                  row_num == 3 ~ prev_val,
                  TRUE ~ value
                ),
                Year = case_when(
                  row_num %in% c(3,4) ~ Year + 10,
                  TRUE ~ Year)
              ) %>% 
              select(Year, value)

black_df <- black_df %>% 
          bind_rows(tibble(value = c(0, 2.2, 0, 0, 0), Year = c(1860, 1860, 1864, 1870, 1870)))
  

red_df <- input_long %>% 
            filter(status == "Free") %>% 
            mutate(prev_val = lead(value)) %>% 
            filter(!is.na(prev_val))


red_df <- red_df %>%
  mutate(freq = 4) %>% 
  uncount(freq) %>% 
  group_by(Year) %>% 
  mutate(row_num = row_number()) %>% 
  filter(Year < 1860)


red_df <- red_df %>%
  mutate(
    value = case_when(
      row_num %in% c(1,4) ~ 3,
      row_num == 3 ~ 3-prev_val,
      TRUE ~ 3-value
    ),
    Year = case_when(
      row_num %in% c(3,4) ~ Year + 10,
      TRUE ~ Year)
  )  %>% 
  select(Year, value)

red_df <- red_df %>% 
  bind_rows(tibble(value = c(3, 2.2, 0, 3), Year = c(1860, 1860, 1864, 1864))) %>% 
  bind_rows(tibble(value = c(3, 0, 0, 3), Year = c(1864, 1864, 1870, 1870)))


pl <- ggplot() +
  geom_polygon(data = black_df, aes(y = Year, x = value, group = 1), fill= "gray7", color = "grey", size = 1) +
  geom_polygon(data = red_df, aes(y = Year, x = value, group = 1), fill= "#dc143c") +
  geom_segment(data = segm_df, aes(x = start, xend = end, y = Year, yend= Year), color = "white", size = 1) + 
  geom_text(data = value_df, aes(x = 3.2, label = value, y = Year), size = 10) +
  scale_y_reverse(labels = ifelse(red_df$Year == 1864, "", as.character(red_df$Year)), breaks = as.numeric(red_df$Year), expand = c(0,0)) + 
  scale_x_continuous(position = "top", breaks = c(0.04,1.04,2.04), labels = c("3%", "2%", "1%"), expand = c(0,0.08)) +
  labs(title = "SLAVES AND FREE BLACKS.") +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 22),
        plot.margin = unit(c(1.5,1,1,1), "lines"),
        plot.title = element_text(vjust = 1, hjust = 0.3, size = 38),
        plot.background = element_rect(fill = "#d2b48c", color = NA),
        panel.grid = element_blank(),
        text = element_text(family = "Psans")) +
  annotate("text", x = 3.2, y = 1785, label = "PERCENT\n OF\n FREE BLACKS", size = 8, lineheight = 0.3) +
  expand_limits(x = 3.5) +
  coord_cartesian(ylim = c(1875, 1790), clip = 'off')

ggsave("Rplot.png", width = 4.7, height = 6, dpi = 300)

