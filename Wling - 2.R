library(tidyverse)
library(ggrepel)
library(dslabs)
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10)

top %>% as_tibble()
Master %>% as_tibble()
top
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

top_salary  <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(playerID, nameFirst, nameLast, teamID, HR, salary)

length(setdiff(AwardsPlayers$playerID,top_names$playerID))



######2.3


library(tidyverse)
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)


nodes <- html_nodes(h, "table")
html_text(nodes[[2]])
head(html_table(nodes[[2]]))


####3
tab1 <- html_table(nodes[[10]], header = TRUE) %>% .[,-1]
head(tab1)

tab2 <- html_table(nodes[[19]], header = TRUE)
head(tab2)

tab3 <- full_join(tab1, tab2, by = "Team")


####4
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
nodes <- html_nodes(h, "table")

####5
head(html_table(nodes[[5]], fill = TRUE))


































