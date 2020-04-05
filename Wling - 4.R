
library(tidyverse)
library(dslabs)
library(lubridate)
options(digits = 3) 
data(brexit_polls)
data(movielens)

class(brexit_polls$startdate)

April <- brexit_polls %>% filter(month(startdate) == "4")

brexit_polls$enddate <- round_date(brexit_polls$enddate, unit = "week", week_start = 7)

June <- brexit_polls %>% filter(month(enddate) == "6" & day(enddate) == "12")
length(June)


table(weekdays(brexit_polls$enddate, abbreviate = FALSE))


movielens$timestamp <- as_datetime(movielens$timestamp)

View(table(hour(movielens$timestamp)))


##############################################

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
data("gutenberg_metadata")
gm <- gutenberg_metadata

sum(str_detect(gm$title, c("Pride and Prejudice")), na.rm = T)

gutenberg_works(gm$title == c("Pride and Prejudice"), languages = "en")

words <-  gutenberg_download(1342) %>% unnest_tokens(word, text)
nrow(words)



## * <- Cero o mas
## ? <- Ninguno o uno
## + <- uno o mas

words <- words %>% 
  filter(!word %in% stop_words$word & !str_detect(word, "^\\d+"))


Best_words <- words %>% count(word) %>% arrange(-n)
nrow(filter(Best_words, n>100))


library("textdata")
afinn <- get_sentiments("afinn")

sent <- words %>% inner_join(afinn, by = "word")
nrow(sent)

sent_plus <- sent$value >= 1
sum(sent_plus)

3414/6065





















