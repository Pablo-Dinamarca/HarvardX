library(HistData)
data("GaltonFamilies")
set.seed(1983)
library(tidyverse)


galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}


beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))


results %>% ggplot(aes(beta1, rss)) + 
  geom_line(col=2)

results %>% filter(rss == min(rss))

library(Lahman)

Y <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(Runs = R/G, Base = BB/G, Home_run =  HR/G)

lm(Runs~Base+Home_run, Y) %>% .$coef



B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# Análisis
# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
Y_hat$residual.scale

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% 
  bind_cols(father = galton_heights$father)

data

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))
#################### Hasta Acá

set.seed(1989)
library(HistData)
data("GaltonFamilies")
options(digits = 3)
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight) 

fit <- lm(mother~daughter, female_heights)
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
Y_hat$fit[1]
female_heights$mother[1]




library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_03 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% group_by(playerID) %>%
  summarise(mean_singles = mean(singles), mean_bb = mean(bb))

View(bat_03)

sum(bat_03$mean_singles > 0.2)
sum(bat_03$mean_bb > 0.2)


bat_04 <- inner_join(bat_02, bat_03)
cor(bat_04$bb, bat_04$mean_bb)


p1 <- bat_04 %>% ggplot(aes(singles, mean_singles)) + geom_point()
p2 <- bat_04 %>% ggplot(aes(bb, mean_bb)) + geom_point()
grid.arrange(p1, p2)

lm(bb ~ mean_bb, data = bat_04)





library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1)

galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>% group_by(pair) %>% 
  summarise(C = cor(parentHeight, childHeight))


library(broom)

Y <- galton %>% group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))

galton %>% group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>% 
  filter(term == "parentHeight") %>% 
  select(pair, estimate, conf.low, conf.high)















  
