library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
View(Teams)

?Teams

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(W_per_game = W/G, E_per_game = E/G) %>% 
  ggplot(aes(W_per_game, E_per_game)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>% 
  ggplot(aes(X3B_per_game, X2B_per_game)) +
  geom_point(alpha = 0.5)


set.seed(1989)
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%    
  filter(gender == "female") %>%    
  group_by(family) %>%    
  sample_n(1) %>%    
  ungroup() %>%    
  select(mother, childHeight) %>%    
  rename(daughter = childHeight)

P_M <- mean(female_heights$mother)
Sd_M <- sd(female_heights$mother)

P_D <- mean(female_heights$daughter)
Sd_D <- sd(female_heights$daughter)

p <- cor(female_heights$mother, female_heights$daughter)



(p^2)*100

m <- cor(female_heights$mother, female_heights$daughter)*(Sd_D/Sd_M)
b <- P_D - m*P_M
m
b


b + m*60
