library(tidyverse)
options(digits = 3)

library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

p <- 0.481


june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt((x_hat*(1-x_hat))/samplesize), 
         d_se_xhat = 2*(sqrt((x_hat*(1-x_hat))/samplesize)),
         lower = spread - qnorm(0.975)*d_se_xhat,
         upper = spread + qnorm(0.975)*d_se_xhat,
         hit = ifelse(lower <= -0.038 & upper >= -0.038, 1, 0))

View(june_polls)

1-mean(june_polls$hit)

###################################################################

x <- june_polls %>% group_by(pollster) %>% 
  summarise(avg = mean(hit),
            N = n()) %>% arrange(-N)
x
mean(x$avg)



###################################################################

june_polls %>% ggplot(aes(poll_type, spread)) + geom_boxplot()

###################################################################

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2, 
            d_se_xhat = 2*(sqrt((p_hat*(1-p_hat))/N)),
            lower = spread - qnorm(0.975)*d_se_xhat,
            upper = spread + qnorm(0.975)*d_se_xhat
            )
combined_by_type









