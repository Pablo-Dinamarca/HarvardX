library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3)
View(stars)

sd(stars$magnitude)

stars %>%
  ggplot(aes(temp, magnitude, col=type, label = star)) +
  geom_point() + geom_text(nudge_x = 1)

stars %>%
  ggplot(aes(log10(temp), magnitude, col=type, label = star)) +
  geom_point() + geom_text() +
  scale_x_reverse() +
  scale_y_reverse() + geom_text_repel()

stars %>%
  ggplot(aes(log10(temp), magnitude, col=type, size = 100)) +
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse()

count(stars %>% filter(temp>8000 & magnitude>10))
















