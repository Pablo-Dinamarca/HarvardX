library(tidyverse)
library(tidyr)

tab <- data.frame(x = c("5'10", "6'1"))
tab

tab %>% separate(x, c("feet", "inches"), sep = "'")

tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

tab2 <- data.frame(x = c("5'10", "6'1\"","5'8inches"))

tab2 %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab2 %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)

extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")    



s <- c("5", "6", "5", "5'", "5''", "5'4")


str_replace(s, "^([4-7])$", "\\1'0")




##Ejercitario 1
tidy <- schedule %>% mutate(staff = str_split(staff, ", | and ")) %>% unnest()
tidy

schedule <- data.frame(day = c("Monday", "Tuesday"), staff = c("Mandy, Chris and Laura", "Steve, Ruth and Frank") )
schedule

str_split(schedule$staff, ", | and ")
str_split(schedule$staff, ",\\s|\\sand\\s")


library(dslabs)
data("gapminder")

dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))


##Ejercitario 2

library(xml2)
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

names(polls) = c("dates", 
                             "remain", 
                             "leave", 
                             "undecided", 
                             "lead", 
                             "samplesize", 
                             "pollster", 
                             "poll_type", 
                             "notes")

names(polls)


polls <- polls %>% .[-1,]

head(polls)

## * <- Cero o mas
## ? <- Ninguno o uno
## + <- uno o mas


polls <- polls %>% extract(remain, c("remain"), "(^\\d+\\.?\\d*[[%]]$)") %>% filter(!is.na(remain))
head(polls$remain)
length(polls$remain)

as.numeric(str_replace(polls$remain, "[[%]]", ""))/100
parse_number(polls$remain)/100




## * <- Cero o mas
## ? <- Ninguno o uno
## + <- uno o mas

temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)])
head(end_date)

temp <- str_extract_all(polls$dates, "[0-9]+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)])
head(end_date)

temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)])
head(end_date)

temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)])
head(end_date)
