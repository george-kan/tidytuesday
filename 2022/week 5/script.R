library(tidyverse)
library(scales)
library(MetBrewer)
library(showtext)
theme_set(theme_minimal())
font_add_google("Poppins", "popins")
showtext_auto()


breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

top_6 <- breed_rank_all %>% filter(`2020 Rank` <= 6) %>% pull(Breed)
breed_traits_6 <- breed_traits %>% filter(gsub("[^[:alnum:]]", "", Breed) %in% gsub("[^[:alnum:]]", "", top_6))


breed_traits_6 <- breed_traits_6 %>% select(-c("Coat Type", "Coat Length"))
breed_6_long <- pivot_longer(breed_traits_6, !Breed, names_to="Trait")

plot_df <- inner_join(breed_6_long, trait_description, by="Trait")
plot_df <- plot_df %>% 
              mutate(Trait_1 = replace(Trait_1, Trait == "Coat Grooming Frequency", "Monthly grooming"), 
                     Trait_5 = replace(Trait_5, Trait == "Coat Grooming Frequency", "Daily grooming"),
                     Trait_1 = replace(Trait_1, Trait == "Barking Level", "Barking Only To Alert"))

ggplot(plot_df, aes(x = value, y = factor(Trait))) +
  geom_jitter(aes(fill = Breed), shape = 21, height = 0.1, width=0.2, size = 4, alpha=0.6) +
  geom_segment(aes(x = 0, xend = 5, yend = factor(Trait)), linetype = "dashed", color = "grey") +
  geom_label(aes(x = 0.6, label = Trait_1), hjust = 1, label.size = NA) +
  geom_label(aes(x = 5.3, label = Trait_5), hjust = 0, label.size = NA) +
  scale_x_continuous(limits = c(-2, 8)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(title = "Report card for the top 6 ranked dog breeds") +
  scale_fill_manual(values =  met.brewer("Klimt", 6)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.title = element_text(face = "plain", hjust = 0.5, size = 18, family = "popins"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())


                                          
                                    