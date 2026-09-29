library(tidyverse)

penguins <- read.csv('data/penguins.csv')
head(penguins)

penguins_not2007 <- penguins %>% filter(year != 2007)
penguins %>% filter(species%in% c("Gentoo", "Chinstrap")) #row selecting
penguins %>% select(species, island, flipper_length_mm) #colums selecting

penguins %>% 
  filter(year == 2007) %>% 
  select(species, island)

penguin_kg <- penguins %>% 
  mutate(body_mass_kg = body_mass_g / 1000) #for new collumn or add information in a already excisting one)

penguin_test <- penguins %>% 
  mutate(species_island = paste(species, island, sep="-")) #to connect two collumns, two informations together
#so 2 or more and then sep with what you would like to seperate them. 

##barplot part
penguins_summary <- penguins %>% 
  group_by(species) %>%  #treat them as groups
  summarize(mean_bodymass = mean(body_mass_g, na.rm = TRUE),
            se = sd(body_mass_g, na.rm = TRUE)/sqrt(n()))

penguins_summary %>% 
  ggplot(aes(x = species, y = mean_bodymass)) +
  geom_col() + 
  geom_errorbar(aes(ymin = mean_bodymass - se, ymax = mean_bodymass + se), width = 0.2)

penguins %>% 
  ggplot(aes(x = species, y = body_mass_g))+
  geom_violin() + #dit kan barplot of violin zijn afhankelijk van wat je wilt
  geom_jitter(width = 0.2, alpha = 0.5) #voor elke enkele pinguin, alfa staat voor transparantie






  
  
  
  
  