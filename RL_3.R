###############################
N <- 25
g <- 1000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))


res <- sim_data %>% 
  group_by(group) %>% do(tidy(lm(y~x, data = .))) %>% filter(term == "x")

sum(res$p.value <= 0.05)*1000

###############################
library(tidyverse)
library(Lahman)
library(broom)

fit <- Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB, HR = HR,  R = R) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)


fit <- Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  mutate(BB = BB, HR = HR,  R = R) %>% group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>% 
  filter(term == "BB")


tidy(lm(estimate~yearID, data = fit), conf.int = TRUE)

fit %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() + geom_smooth(method = "lm")

####################################

library(tidyverse)
library(broom)
library(Lahman)

Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G, 
           R = R/G,
           HR = HR/G, 
           W = round(W/10)) %>% filter(W %in% 5:10) %>% 
  group_by(W) %>% 
    filter(n() >= 20)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small %>% 
  do(tidy(lm(avg_attendance ~ HR, data = .), conf.int = TRUE)) %>% 
  filter(term == "HR")


tidy(lm(avg_attendance ~ W, data = Teams_small))

cor(Teams_small$W, Teams_small$R)
cor(Teams_small$W, Teams_small$HR)

tidy(lm(avg_attendance ~ W, data = Teams_small))


##################Q4
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, 
         R = R/G,
         HR = HR/G)

fit <- lm(avg_attendance ~ W + R + HR + yearID, data = Teams_small)
tidy(fit)

##################Q5
predict(fit, data.frame(R = 5, HR = 1.2, W = 80, yearID = 2002))


##################Q6
Teams_small <- Teams %>% 
  filter(yearID == 2002) %>% 
  mutate(avg_attendance = attendance/G, 
         R = R/G,
         HR = HR/G)

fit <- lm(avg_attendance ~ W + R + HR + yearID, data = Teams_small)
tidy(fit)

Teams_small2 <- Teams_small %>% mutate(Y_hat = predict(fit, newdata = .))

Teams_big <- Teams %>% 
  filter(yearID == 2002) %>% 
  mutate(avg_attendance = attendance/G)


cor(Teams_small2$Y_hat, Teams_big$avg_attendance)
#####################################
#Section 3
library(tidyverse)
library(dslabs)
data("research_funding_rates")
research_funding_rates


totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)




P <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))


(P$men[2]/sum(P$men))*100

(P$women[2]/sum(P$women))*100


Chist <- P %>% select(-awarded) %>% chisq.test()
tidy(Chist)


dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")

dat  %>%
  ggplot(aes(success, discipline, color=gender, size = applications)) + 
  geom_point()







