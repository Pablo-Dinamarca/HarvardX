library(tidyverse)
options(digits = 3)

library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

p <- 0.481
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)


Table <- brexit_hit %>% group_by(poll_type, hit) %>% 
  summarise(num = n()) %>% spread(poll_type, num)

P_Value <- Table %>% select(-hit) %>%
  chisq.test()

Table

Online <- (Table[2,2]/sum(Table[2]))/(Table[1,2]/sum(Table[2]))
Online

Tele <- (Table[2,3]/sum(Table[3]))/(Table[1,3]/sum(Table[3]))
Tele

Online/Tele

R_Pr <- (Table[1,2]*Table[2,3])/(Table[2,2]*Table[1,3])
R_Pr

brexit_polls %>% ggplot(aes(enddate, spread, color = poll_type)) + 
  geom_smooth(method = "loess", span = 0.4) + geom_point() + 
  geom_hline(yintercept = -0.038)

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

View(brexit_long)

brexit_long %>% ggplot(aes(enddate, proportion, color = vote)) + 
  geom_smooth(method = "loess", span = 0.3)









