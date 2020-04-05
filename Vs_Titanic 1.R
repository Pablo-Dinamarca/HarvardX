options(digits = 3)
library(tidyverse)
library(titanic)
library(ggplot2)


titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#1
titanic %>% group_by(Sex) %>% 
  ggplot(aes(Age, fill = Sex)) + geom_density(alpha=0.2)


#2
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
params

titanic %>% 
  ggplot(aes(sample = Age)) + geom_qq(dparams = params) + geom_abline()

#3
titanic %>% group_by(Sex) %>% 
  ggplot(aes(Survived, fill = Sex)) + 
  geom_bar(alpha = 0.5, position = position_dodge())



#4
titanic %>% group_by(Age) %>% 
  ggplot(aes(Age, fill = Survived)) + geom_density(alpha=0.2)

#5
titanic %>% filter(!is.na(Fare)) %>% 
  ggplot(aes(Age, Fare, fill = Survived)) + 
  geom_boxplot(alpha = 0.2) + 
  scale_y_continuous(trans="log2")

#6
titanic %>% group_by(Pclass) %>% 
  ggplot(aes(Pclass, fill = Survived)) + geom_bar(alpha=0.5)

titanic %>% group_by(Pclass) %>% 
  ggplot(aes(Pclass, fill = Survived)) + 
  geom_bar(alpha=0.5, position = position_fill())

titanic %>% group_by(Survived) %>% 
  ggplot(aes(Survived, fill = Pclass)) + 
  geom_bar(alpha=0.5, position = position_fill())

#7
titanic %>% group_by(Age) %>% 
  ggplot(aes(Age, fill = Survived)) + 
  geom_density(alpha = 0.2) + facet_grid(Sex~Pclass)
