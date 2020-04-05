library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)


a <- death_prob %>% filter(age == 50 & sex == "Female") 
Pr_50F <- a$prob
Pr_50F

Es_50F <- ((1150*(1-Pr_50F)) + (-150000*(Pr_50F)))*1000
Es_50F

Sd_50F <-  (abs(1150+150000)*sqrt((Pr_50F*(1-Pr_50F))))*sqrt(1000)
Sd_50F

CLT_50F <- pnorm(0, Es_50F, Sd_50F)
CLT_50F




b <- death_prob %>% filter(age == 50 & sex == "Male") 
Pr_50M <- b$prob
Pr_50M

Money <- ((700000/1000)-(-150000*(Pr_50M)))/(1-Pr_50M)
Es_50M <- ((Money*(1-Pr_50M)) + (-150000*(Pr_50M)))*1000
Es_50M

Sd_50M <-  (abs(Money+150000)*sqrt((Pr_50M*(1-Pr_50M))))*sqrt(1000)
Sd_50M

CLT_50M <- pnorm(0, Es_50M, Sd_50M)
CLT_50M




Pr_50 <- 0.015
Pr_50

Es_50 <- ((1150*(1-Pr_50)) + (-150000*(Pr_50)))*1000
Es_50

Sd_50 <-  (abs(1150+150000)*sqrt((Pr_50*(1-Pr_50))))*sqrt(1000)
Sd_50

CLT_50 <- pnorm(-1000000, Es_50, Sd_50)
CLT_50


P <- seq(.01, .03, .0025)
P

Es <- ((1150*(1-P)) + (-150000*(P)))*1000
Es

Sd <-  (abs(1150+150000)*sqrt((P*(1-P))))*sqrt(1000)
Sd

CLT <- pnorm(-1000000, Es, Sd)
CLT






set.seed(25)
p_loss <- 0.015

X <- sample(c(1150, -150000), replace = TRUE, 1000, prob = c((1-p_loss), p_loss))
sum(X)/1000000


set.seed(27)
Queen <- replicate(10000, {
  X <- sample(c(1150, -150000), replace = TRUE, 1000, prob = c((1-p_loss), p_loss))
  sum(X)
})
mean(Queen >= 0)





