library(tidyverse)
options(digits = 3)

library(dslabs)
data(brexit_polls)
view(brexit_polls)

p <- 0.481
d <- 2*p-1
n <- 1500

Es <- p*n
Es

Sd <- sqrt(n*p*(1-p))
Sd

Es_Xhat <- p

Sd_Xhat <- sqrt((Es_Xhat*(1-Es_Xhat))/n)
Sd_Xhat

Es_d <- 2*p-1

Sd_d <- 2*sqrt(p*(1-p)/n)
Sd_d

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

View(brexit_polls)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

Es_Spread <- mean(brexit_polls$spread)

Sd_Spread <-  sd(brexit_polls$spread)
Sd_Spread

Pr <- brexit_polls[1, 10]
n <- brexit_polls[1, 5]
brexit_polls[1,]

c(brexit_polls[1,10] - qnorm(0.975)*sqrt((Pr*(1-Pr))/n), brexit_polls[1,10] + qnorm(0.975)*sqrt((Pr*(1-Pr))/n))















